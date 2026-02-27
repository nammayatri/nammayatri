#!/usr/bin/env python3
"""
Optimized build log analyzer for Nix builds with Haskell compilation tracking.
"""

import re
import sys
import json
from datetime import datetime
from collections import defaultdict


class BuildAnalyzer:
    def __init__(self, log_file):
        self.log_file = log_file
        self.builds = {}
        self.module_compilations = {}
        self.cache_stats = defaultdict(lambda: {'hits': 0, 'misses': 0})
        self.cache_hits = []
        self.pending_cache_queries = {}  # store_path -> [cache_names]
        self.current_compilations = {}
        self.events_count = 0
        self.compilation_count = 0
        self.cache_event_count = 0

    def parse_timestamp(self, ts_str):
        """Parse GitHub Actions timestamp format: 2026-02-24T13:38:19.5012063Z"""
        try:
            return datetime.strptime(ts_str[:19], "%Y-%m-%dT%H:%M:%S")
        except:
            return None

    def extract_package_name(self, drv_path):
        """Extract package name from derivation path.

        Extracts package name without version for consistent matching.
        e.g., 'rider-app-source-0.1.0.0' -> 'rider-app-source'
        """
        # Get the filename without path and .drv extension
        filename = drv_path.split('/')[-1].replace('.drv', '')
        # Remove the hash prefix (32 characters + first dash)
        name = filename[33:] if len(filename) > 33 else filename
        # Remove version suffix (-X.Y.Z or -X.Y.Z.W) - version must start with digit
        # Don't remove things like "-source", "-lib", etc.
        name = re.sub(r'-\d+\.\d+\.\d+(?:\.\d+)?$', '', name)
        return name

    def extract_cache_name(self, url):
        """Extract cache name from URL."""
        if 'cache.nixos.org' in url:
            return 'cache.nixos.org'
        elif 'cache.nixos.asia' in url:
            return 'cache.nixos.asia'
        elif 'nammayatri.cachix.org' in url:
            return 'nammayatri.cachix.org'
        return url

    def parse_log(self):
        """Parse the build log using efficient line-by-line processing."""
        print("📊 Parsing build log...")

        # Pre-compiled regex patterns for performance
        ts_pattern = re.compile(r'^(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+Z)')
        build_start_pattern = re.compile(r"building '(/nix/store/[^']+)'")
        build_end_pattern = re.compile(r'(\S+)>\s+post-installation fixup')
        comp_pattern = re.compile(r'(\S+)>\s*\[(\d+)\s+of\s+(\d+)\]\s+Compiling\s+(\S+)')
        linking_pattern = re.compile(r'(\S+)>\s+Linking\s')
        cache_query_pattern = re.compile(r"querying info about '(/nix/store/[^']+)'\s+on\s+'([^']+)'")
        cache_download_pattern = re.compile(r"downloading\s+'https://([^/]+)/(.+\.narinfo)'")
        derivations_pattern = re.compile(r'these (\d+) derivations will be built')

        build_starts = {}
        local_build_count = 0
        last_timestamp = None

        with open(self.log_file, 'r') as f:
            for line in f:
                line = line.rstrip('\n')
                if not line:
                    continue

                # Extract timestamp
                ts_match = ts_pattern.match(line)
                if not ts_match:
                    continue

                timestamp = self.parse_timestamp(ts_match.group(1))
                if not timestamp:
                    continue

                # Track the last timestamp seen in the log
                last_timestamp = timestamp

                content = line[ts_match.end():].lstrip()

                # Fast path: check for common patterns first
                # Build start
                if "building '/nix/store/" in content:
                    match = build_start_pattern.search(content)
                    if match:
                        drv_path = match.group(1)
                        package = self.extract_package_name(drv_path)
                        # Skip *-source packages
                        if package.endswith('-source'):
                            continue
                        build_starts[package] = (timestamp, drv_path)
                        local_build_count += 1
                        self.events_count += 1
                    continue

                # Build end
                if "post-installation fixup" in content:
                    match = build_end_pattern.search(content)
                    if match:
                        end_package = match.group(1)
                        # Skip *-source packages
                        if end_package.endswith('-source'):
                            continue
                        # Find matching start by normalized package name
                        # The end log uses short name, start uses drv path
                        for pkg, (start_time, drv_path) in list(build_starts.items()):
                            # Check if the end package name is a prefix or matches
                            if end_package in drv_path or drv_path.endswith(f"-{end_package}.drv") or drv_path.endswith(f"-{end_package}-"):
                                duration = (timestamp - start_time).total_seconds()
                                self.builds[pkg] = {
                                    'start_time': start_time.isoformat(),
                                    'end_time': timestamp.isoformat(),
                                    'duration_seconds': duration,
                                    'duration_human': self.format_duration(duration)
                                }
                                del build_starts[pkg]
                                self.events_count += 1
                                break
                    continue

                # Haskell compilation - skip *-source packages
                if "Compiling" in content and "> [" in content:
                    match = comp_pattern.search(content)
                    if match:
                        package = match.group(1)

                        # Skip *-source packages (they are source bundles, not actual compilation)
                        if package.endswith('-source'):
                            continue

                        idx = int(match.group(2))
                        total = int(match.group(3))
                        module = match.group(4)

                        # Close previous compilation for this package (if any)
                        if package in self.current_compilations:
                            prev = self.current_compilations[package]
                            duration = (timestamp - prev['timestamp']).total_seconds()
                            key = f"{package}:{prev['module']}"
                            self.module_compilations[key] = {
                                'package': package,
                                'module': prev['module'],
                                'start_time': prev['timestamp'].isoformat(),
                                'end_time': timestamp.isoformat(),
                                'duration_seconds': duration,
                                'duration_human': self.format_duration(duration)
                            }

                        # Track the start of this compilation
                        self.current_compilations[package] = {
                            'timestamp': timestamp,
                            'module': module,
                            'index': idx,
                            'total': total
                        }
                        self.compilation_count += 1
                    continue

                # Linking marks the end of the last module compilation
                if "Linking" in content:
                    match = linking_pattern.search(content)
                    if match:
                        package = match.group(1)

                        # Skip *-source packages
                        if package.endswith('-source'):
                            continue

                        # Close the last compilation for this package
                        if package in self.current_compilations:
                            prev = self.current_compilations[package]
                            duration = (timestamp - prev['timestamp']).total_seconds()
                            key = f"{package}:{prev['module']}"
                            self.module_compilations[key] = {
                                'package': package,
                                'module': prev['module'],
                                'start_time': prev['timestamp'].isoformat(),
                                'end_time': timestamp.isoformat(),
                                'duration_seconds': duration,
                                'duration_human': self.format_duration(duration)
                            }
                            del self.current_compilations[package]
                    continue

                # Cache hit: downloading narinfo means cache has the path
                if "downloading" in content and ".narinfo" in content:
                    match = cache_download_pattern.search(content)
                    if match:
                        cache_name = self.extract_cache_name(match.group(1))
                        narinfo_path = match.group(2)

                        # Record the hit for this cache
                        self.cache_stats[cache_name]['hits'] += 1
                        self.cache_hits.append({
                            'cache': cache_name,
                            'path': narinfo_path,
                            'timestamp': timestamp.isoformat()
                        })
                        self.cache_event_count += 1
                    continue

        # Close any remaining compilations using the last timestamp from the log
        # Skip *-source packages
        if self.current_compilations and self.compilation_count > 0 and last_timestamp:
            for package, current in list(self.current_compilations.items()):
                if package.endswith('-source'):
                    continue
                duration = (last_timestamp - current['timestamp']).total_seconds()
                key = f"{package}:{current['module']}"
                if key not in self.module_compilations:
                    self.module_compilations[key] = {
                        'package': package,
                        'module': current['module'],
                        'start_time': current['timestamp'].isoformat(),
                        'end_time': last_timestamp.isoformat(),
                        'duration_seconds': duration,
                        'duration_human': self.format_duration(duration)
                    }

        # Handle incomplete builds
        for package, (start_time, drv_path) in build_starts.items():
            if self.events_count > 0:
                duration = 0  # We don't have end time
                self.builds[package] = {
                    'start_time': start_time.isoformat(),
                    'end_time': None,
                    'duration_seconds': 0,
                    'duration_human': 'unknown (incomplete)',
                    'incomplete': True
                }

        self.cache_stats['local_build'] = {'count': local_build_count}

        print(f"✅ Parsed {self.events_count} build events")
        print(f"✅ Parsed {self.compilation_count} compilation events")
        print(f"✅ Parsed {self.cache_event_count} cache events")

    def format_duration(self, seconds):
        """Format duration in human readable form."""
        if seconds < 60:
            return f"{seconds:.1f}s"
        elif seconds < 3600:
            return f"{seconds/60:.1f}m"
        else:
            return f"{seconds/3600:.1f}h {(seconds % 3600)/60:.1f}m"

    def calculate_summary(self):
        """Calculate summary statistics."""
        completed_builds = [b for b in self.builds.values() if b.get('end_time')]

        if not completed_builds:
            return None

        durations = [b['duration_seconds'] for b in completed_builds]
        start_times = [datetime.fromisoformat(b['start_time']) for b in completed_builds]
        end_times = [datetime.fromisoformat(b['end_time']) for b in completed_builds]

        wall_clock = (max(end_times) - min(start_times)).total_seconds()
        total_cpu = sum(durations)

        return {
            'wall_clock_seconds': wall_clock,
            'wall_clock_human': self.format_duration(wall_clock),
            'total_cpu_seconds': total_cpu,
            'total_cpu_human': self.format_duration(total_cpu),
            'parallelization_factor': total_cpu / wall_clock if wall_clock > 0 else 0,
            'start_time': min(start_times).isoformat(),
            'end_time': max(end_times).isoformat()
        }

    def print_summary(self):
        """Print comprehensive analysis summary."""
        summary = self.calculate_summary()

        print("\n" + "="*80)
        print("📈 BUILD ANALYSIS SUMMARY")
        print("="*80)

        if summary:
            print(f"\n⏰ OVERALL TIMING:")
            print(f"   Wall Clock Time: {summary['wall_clock_human']}")
            print(f"   Total CPU Time:  {summary['total_cpu_human']}")
            print(f"   Parallelization: {summary['parallelization_factor']:.1f}x")

        # Cache statistics
        print(f"\n💾 CACHE STATISTICS:")
        for cache, stats in self.cache_stats.items():
            if isinstance(stats, dict):
                if 'hits' in stats:
                    print(f"   {cache}: {stats['hits']} hits, {stats.get('misses', 0)} misses")
                elif 'count' in stats:
                    print(f"   {cache}: {stats['count']} builds")

        # Top longest package builds
        print(f"\n🐌 LONGEST PACKAGE BUILD TIMES:")
        sorted_builds = sorted(self.builds.items(),
                              key=lambda x: x[1].get('duration_seconds', 0), reverse=True)
        for i, (package, data) in enumerate(sorted_builds[:10]):
            status = " (INCOMPLETE)" if data.get('incomplete') else ""
            print(f"   {i+1:2d}. {package:<45} {data['duration_human']:>10}{status}")

        # Top longest module compilations
        if self.module_compilations:
            print(f"\n🐌 LONGEST MODULE COMPILATION TIMES:")
            sorted_modules = sorted(self.module_compilations.items(),
                                   key=lambda x: x[1]['duration_seconds'], reverse=True)
            for i, (key, data) in enumerate(sorted_modules[:15]):
                module_short = data['module'][-35:] if len(data['module']) > 35 else data['module']
                print(f"   {i+1:2d}. {data['package']:<20} {module_short:<35} {data['duration_human']:>10}")

        # Build statistics
        print(f"\n📊 BUILD STATISTICS:")
        print(f"   Total Packages:           {len(self.builds)}")
        print(f"   Total Modules Compiled:   {len(self.module_compilations)}")
        print(f"   Cache Hits:               {len(self.cache_hits)}")

    def export_json(self, filename):
        """Export analysis results to JSON."""
        data = {
            'summary': self.calculate_summary(),
            'builds': self.builds,
            'module_compilations': self.module_compilations,
            'cache_stats': dict(self.cache_stats),
            'cache_hits': self.cache_hits
        }

        with open(filename, 'w') as f:
            json.dump(data, f, indent=2, default=str)

        print(f"\n💾 Detailed analysis exported to: {filename}")


def main():
    if len(sys.argv) != 2:
        print("Usage: python3 analyze_build_log.py <build_log_file>")
        sys.exit(1)

    log_file = sys.argv[1]
    analyzer = BuildAnalyzer(log_file)

    try:
        analyzer.parse_log()
        analyzer.print_summary()
        analyzer.export_json("build_analysis.json")
    except Exception as e:
        print(f"❌ Error analyzing build log: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
