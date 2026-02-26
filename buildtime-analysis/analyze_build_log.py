#!/usr/bin/env python3

import re
import sys
from datetime import datetime
from collections import defaultdict, namedtuple
import json

# Data structures
BuildEvent = namedtuple(
    'BuildEvent', ['timestamp', 'event_type', 'package', 'details'])
BuildPhase = namedtuple(
    'BuildPhase', ['package', 'start_time', 'end_time', 'duration_seconds', 'phase'])


class BuildAnalyzer:
    def __init__(self, log_file):
        self.log_file = log_file
        self.events = []
        self.builds = {}
        self.parallel_groups = []

    def parse_timestamp(self, ts_str):
        """Parse GitHub Actions timestamp"""
        # Format: 2026-02-24T13:38:19.5012063Z
        return datetime.strptime(ts_str.split('.')[0], "%Y-%m-%dT%H:%M:%S")

    def extract_package_name(self, drv_path):
        """Extract package name from derivation path"""
        # /nix/store/hash-package-name-version.drv -> package-name
        match = re.search(
            r'/nix/store/[a-z0-9]+-(.+?)(?:-\d+\.\d+\.\d+(?:\.\d+)?)?\.drv', drv_path)
        if match:
            return match.group(1)
        return drv_path.split('/')[-1].replace('.drv', '')

    def parse_log(self):
        """Parse the build log and extract events"""
        print("📊 Parsing build log...")

        with open(self.log_file, 'r') as f:
            for line_num, line in enumerate(f, 1):
                line = line.strip()
                if not line:
                    continue

                # Extract timestamp
                ts_match = re.match(
                    r'(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+Z)', line)
                if not ts_match:
                    continue

                timestamp = self.parse_timestamp(ts_match.group(1))

                # Build start events
                if "building '/nix/store/" in line:
                    drv_match = re.search(
                        r"building '(/nix/store/[^']+)'", line)
                    if drv_match:
                        drv_path = drv_match.group(1)
                        package = self.extract_package_name(drv_path)
                        self.events.append(BuildEvent(
                            timestamp, 'build_start', package, drv_path))

                # Build completion events
                elif "post-installation fixup" in line:
                    pkg_match = re.search(
                        r'(\S+)>\s+post-installation fixup', line)
                    if pkg_match:
                        package = pkg_match.group(1)
                        self.events.append(BuildEvent(
                            timestamp, 'build_end', package, ''))

                # Other interesting events
                elif "these " in line and "derivations will be built" in line:
                    count_match = re.search(
                        r'these (\d+) derivations will be built', line)
                    if count_match:
                        count = int(count_match.group(1))
                        self.events.append(BuildEvent(
                            timestamp, 'build_plan', f'{count}_derivations', ''))

        print(f"✅ Parsed {len(self.events)} events")

    def analyze_build_durations(self):
        """Calculate build durations for each package"""
        print("⏱️  Analyzing build durations...")

        # Track start times
        start_times = {}

        for event in self.events:
            if event.event_type == 'build_start':
                start_times[event.package] = event.timestamp
            elif event.event_type == 'build_end':
                if event.package in start_times:
                    start_time = start_times[event.package]
                    duration = (event.timestamp - start_time).total_seconds()

                    self.builds[event.package] = {
                        'start_time': start_time,
                        'end_time': event.timestamp,
                        'duration_seconds': duration,
                        'duration_minutes': duration / 60,
                        'duration_human': self.format_duration(duration)
                    }

                    del start_times[event.package]

        # Handle incomplete builds (started but not finished)
        for package, start_time in start_times.items():
            if self.events:
                last_event_time = max(e.timestamp for e in self.events)
                duration = (last_event_time - start_time).total_seconds()

                self.builds[package] = {
                    'start_time': start_time,
                    'end_time': None,
                    'duration_seconds': duration,
                    'duration_minutes': duration / 60,
                    'duration_human': self.format_duration(duration) + ' (incomplete)',
                    'incomplete': True
                }

    def format_duration(self, seconds):
        """Format duration in human readable form"""
        if seconds < 60:
            return f"{seconds:.1f}s"
        elif seconds < 3600:
            return f"{seconds/60:.1f}m"
        else:
            return f"{seconds/3600:.1f}h {(seconds % 3600)/60:.1f}m"

    def identify_parallel_builds(self):
        """Identify which builds happened in parallel"""
        print("🔄 Analyzing parallel builds...")

        # Get all ongoing builds at each point in time
        ongoing_builds = []

        for event in sorted(self.events, key=lambda x: x.timestamp):
            if event.event_type == 'build_start':
                ongoing_builds.append(event.package)
            elif event.event_type == 'build_end' and event.package in ongoing_builds:
                ongoing_builds.remove(event.package)

            # If we have multiple builds ongoing, record this parallel group
            if len(ongoing_builds) > 1:
                parallel_group = {
                    'timestamp': event.timestamp,
                    'packages': ongoing_builds.copy(),
                    'count': len(ongoing_builds)
                }

                # Only add if this is a new combination
                if not any(set(pg['packages']) == set(parallel_group['packages'])
                           for pg in self.parallel_groups):
                    self.parallel_groups.append(parallel_group)

    def get_build_timeline(self):
        """Get chronological build timeline"""
        timeline = []
        for event in sorted(self.events, key=lambda x: x.timestamp):
            if event.event_type in ['build_start', 'build_end']:
                timeline.append(event)
        return timeline

    def analyze_total_time(self):
        """Calculate total build time vs wall clock time"""
        if not self.events:
            return None

        start_time = min(
            e.timestamp for e in self.events if e.event_type == 'build_start')
        end_time = max(
            e.timestamp for e in self.events if e.event_type == 'build_end')

        wall_clock_time = (end_time - start_time).total_seconds()

        # Calculate total CPU time (sum of all build durations)
        total_cpu_time = sum(build['duration_seconds']
                             for build in self.builds.values())

        # Calculate parallelization efficiency
        efficiency = (total_cpu_time /
                      wall_clock_time) if wall_clock_time > 0 else 0

        return {
            'wall_clock_seconds': wall_clock_time,
            'wall_clock_human': self.format_duration(wall_clock_time),
            'total_cpu_seconds': total_cpu_time,
            'total_cpu_human': self.format_duration(total_cpu_time),
            'parallelization_factor': efficiency,
            'start_time': start_time,
            'end_time': end_time
        }

    def print_summary(self):
        """Print comprehensive analysis summary"""
        print("\n" + "="*80)
        print("📈 BUILD ANALYSIS SUMMARY")
        print("="*80)

        # Overall timing
        timing = self.analyze_total_time()
        if timing:
            print(f"\n⏰ OVERALL TIMING:")
            print(f"   Wall Clock Time: {timing['wall_clock_human']}")
            print(f"   Total CPU Time:  {timing['total_cpu_human']}")
            print(
                f"   Parallelization: {timing['parallelization_factor']:.1f}x")
            print(
                f"   Build Period:    {timing['start_time'].strftime('%H:%M:%S')} → {timing['end_time'].strftime('%H:%M:%S')}")

        # Top longest builds
        print(f"\n🐌 LONGEST BUILD TIMES:")
        sorted_builds = sorted(self.builds.items(),
                               key=lambda x: x[1]['duration_seconds'], reverse=True)
        for i, (package, data) in enumerate(sorted_builds[:10]):
            status = " (INCOMPLETE)" if data.get('incomplete') else ""
            print(
                f"   {i+1:2d}. {package:<35} {data['duration_human']:>10}{status}")

        # Parallelization analysis
        print(f"\n⚡ PARALLELIZATION ANALYSIS:")
        max_parallel = max((len(pg['packages'])
                           for pg in self.parallel_groups), default=1)
        print(f"   Maximum Parallel Builds: {max_parallel}")
        print(f"   Parallel Groups Found:   {len(self.parallel_groups)}")

        # Show some parallel groups
        if self.parallel_groups:
            print(f"\n   Sample Parallel Groups:")
            for i, pg in enumerate(self.parallel_groups[:5]):
                packages_str = ", ".join(pg['packages'][:3])
                if len(pg['packages']) > 3:
                    packages_str += f" +{len(pg['packages'])-3} more"
                print(f"     {i+1}. [{pg['count']} builds] {packages_str}")

        # Build phases analysis
        print(f"\n📊 BUILD STATISTICS:")
        total_builds = len(self.builds)
        completed_builds = len(
            [b for b in self.builds.values() if not b.get('incomplete')])
        incomplete_builds = total_builds - completed_builds

        print(f"   Total Packages Built:     {total_builds}")
        print(f"   Completed Builds:         {completed_builds}")
        print(f"   Incomplete Builds:        {incomplete_builds}")

        if self.builds:
            durations = [b['duration_seconds'] for b in self.builds.values()]
            avg_duration = sum(durations) / len(durations)
            print(
                f"   Average Build Time:       {self.format_duration(avg_duration)}")

    def print_detailed_timeline(self, limit=20):
        """Print detailed build timeline"""
        print(f"\n📅 BUILD TIMELINE (showing first {limit} events):")
        print("-" * 80)

        timeline = self.get_build_timeline()
        for i, event in enumerate(timeline[:limit]):
            time_str = event.timestamp.strftime('%H:%M:%S')
            event_type = "🚀 START" if event.event_type == 'build_start' else "✅ END  "
            print(f"   {time_str} {event_type} {event.package}")

        if len(timeline) > limit:
            print(f"   ... and {len(timeline) - limit} more events")

    def export_json(self, filename):
        """Export analysis results to JSON"""
        data = {
            'summary': self.analyze_total_time(),
            'builds': self.builds,
            'parallel_groups': self.parallel_groups,
            'timeline': [
                {
                    'timestamp': event.timestamp.isoformat(),
                    'event_type': event.event_type,
                    'package': event.package,
                    'details': event.details
                }
                for event in self.events
            ]
        }

        with open(filename, 'w') as f:
            json.dump(data, f, indent=2, default=str)

        print(f"\n💾 Detailed analysis exported to: {filename}")


def main():
    if len(sys.argv) != 2:
        print("Usage: python analyze_build_log.py <build_log_file>")
        sys.exit(1)

    log_file = sys.argv[1]

    analyzer = BuildAnalyzer(log_file)

    try:
        analyzer.parse_log()
        analyzer.analyze_build_durations()
        analyzer.identify_parallel_builds()

        analyzer.print_summary()
        analyzer.print_detailed_timeline()

        # Export detailed results
        output_file = "build_analysis.json"
        analyzer.export_json(output_file)

    except Exception as e:
        print(f"❌ Error analyzing build log: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
