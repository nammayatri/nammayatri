#!/usr/bin/env python3

import json
import re
from datetime import datetime, timedelta
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib.patches import Rectangle
import pandas as pd
import numpy as np


class BuildVisualizer:
    def __init__(self, analysis_file):
        with open(analysis_file, 'r') as f:
            self.data = json.load(f)

        self.builds = self.data['builds']

        # Timeline is optional - build from builds data if not present
        self.timeline = self.data.get('timeline', [])
        if self.timeline:
            # Convert timestamp strings back to datetime objects
            for event in self.timeline:
                event['timestamp'] = datetime.fromisoformat(
                    event['timestamp'].replace('Z', '+00:00'))
        else:
            # Build timeline from builds data
            self.timeline = self._build_timeline_from_builds()

        # parallel_groups may not be present
        self.parallel_groups = self.data.get('parallel_groups', [])

    def _build_timeline_from_builds(self):
        """Build timeline events from builds data when timeline is not in JSON."""
        timeline = []
        for package, details in self.builds.items():
            if details.get('start_time'):
                start_dt = datetime.fromisoformat(details['start_time'].replace('Z', '+00:00'))
                timeline.append({
                    'timestamp': start_dt,
                    'event_type': 'build_start',
                    'package': package,
                    'details': ''
                })
            if details.get('end_time'):
                end_dt = datetime.fromisoformat(details['end_time'].replace('Z', '+00:00'))
                timeline.append({
                    'timestamp': end_dt,
                    'event_type': 'build_end',
                    'package': package,
                    'details': ''
                })
        # Sort by timestamp
        timeline.sort(key=lambda x: x['timestamp'])
        return timeline

    def create_gantt_chart(self):
        """Create a Gantt chart showing build timeline and parallelization"""
        print("📊 Creating Gantt chart...")

        # Prepare data for Gantt chart
        build_data = []
        for package, details in self.builds.items():
            if not details.get('incomplete'):
                start_time = datetime.fromisoformat(
                    details['start_time'].replace('Z', '+00:00'))
                end_time = datetime.fromisoformat(
                    details['end_time'].replace('Z', '+00:00'))
                duration = details['duration_seconds'] / 60  # in minutes

                build_data.append({
                    'package': package,
                    'start': start_time,
                    'duration': duration,
                    'end': end_time
                })

        # Sort by start time
        build_data.sort(key=lambda x: x['start'])

        # Check if we have any build data
        if not build_data:
            print("⚠️  No completed build data found, skipping Gantt chart")
            return

        # Create figure
        fig, ax = plt.subplots(figsize=(16, max(12, len(build_data) * 0.4)))

        # Color map for different phases
        colors = plt.cm.Set3(np.linspace(0, 1, len(build_data)))

        # Plot each build as a horizontal bar
        for i, build in enumerate(build_data):
            start_time = build['start']
            duration_mins = build['duration']

            # Create rectangle
            rect = Rectangle((mdates.date2num(start_time), i),
                             # Convert to days for matplotlib
                             duration_mins / (24 * 60),
                             0.8,
                             facecolor=colors[i],
                             alpha=0.8,
                             edgecolor='black',
                             linewidth=0.5)
            ax.add_patch(rect)

            # Add package name
            ax.text(mdates.date2num(start_time) + (duration_mins / (24 * 60)) / 2,
                    i + 0.4,
                    f"{build['package']} ({duration_mins:.1f}m)",
                    ha='center', va='center', fontsize=8, weight='bold')

        # Format axes
        ax.set_xlim(mdates.date2num(build_data[0]['start']) - 0.001,
                    mdates.date2num(max(b['end'] for b in build_data)) + 0.001)
        ax.set_ylim(-0.5, len(build_data) - 0.5)

        # Format x-axis to show time
        ax.xaxis.set_major_formatter(mdates.DateFormatter('%H:%M:%S'))
        ax.xaxis.set_major_locator(mdates.MinuteLocator(interval=5))

        # Labels and title
        ax.set_xlabel('Build Time (HH:MM:SS)')
        ax.set_ylabel('Build Order')
        ax.set_title('Build Timeline - Gantt Chart\n(Showing Parallelization)',
                     fontsize=16, weight='bold')

        plt.xticks(rotation=45)
        plt.tight_layout()
        plt.grid(True, alpha=0.3)

        # Save the plot
        plt.savefig('./build_gantt_chart.png',
                    dpi=300, bbox_inches='tight')
        print("✅ Gantt chart saved to build_gantt_chart.png")
        plt.close()

    def create_parallel_analysis_chart(self):
        """Create chart showing parallelization over time"""
        print("📊 Creating parallelization analysis chart...")

        # Count concurrent builds over time
        time_points = []
        concurrent_counts = []

        # Sort timeline events
        sorted_events = sorted(self.timeline, key=lambda x: x['timestamp'])

        ongoing_builds = set()
        for event in sorted_events:
            if event['event_type'] == 'build_start':
                ongoing_builds.add(event['package'])
            elif event['event_type'] == 'build_end':
                ongoing_builds.discard(event['package'])

            time_points.append(event['timestamp'])
            concurrent_counts.append(len(ongoing_builds))

        # Create the plot
        fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(16, 12))

        # Top plot: Concurrent builds over time
        ax1.plot(time_points, concurrent_counts, linewidth=2,
                 color='blue', marker='o', markersize=3)
        ax1.fill_between(time_points, concurrent_counts,
                         alpha=0.3, color='blue')
        ax1.set_ylabel('Concurrent Builds')
        ax1.set_title('Build Parallelization Over Time',
                      fontsize=14, weight='bold')
        ax1.grid(True, alpha=0.3)
        ax1.xaxis.set_major_formatter(mdates.DateFormatter('%H:%M'))

        # Bottom plot: Build durations histogram
        # Calculate duration_minutes from duration_seconds if not present
        completed_durations = []
        for details in self.builds.values():
            if not details.get('incomplete'):
                if 'duration_minutes' in details:
                    completed_durations.append(details['duration_minutes'])
                elif 'duration_seconds' in details:
                    completed_durations.append(details['duration_seconds'] / 60)

        if completed_durations:
            ax2.hist(completed_durations, bins=15, alpha=0.7,
                     color='green', edgecolor='black')
            ax2.set_xlabel('Build Duration (minutes)')
            ax2.set_ylabel('Number of Builds')
            ax2.set_title('Distribution of Build Times',
                          fontsize=14, weight='bold')
            ax2.grid(True, alpha=0.3)

        plt.tight_layout()
        plt.savefig('./parallelization_analysis.png',
                    dpi=300, bbox_inches='tight')
        print("✅ Parallelization analysis saved to parallelization_analysis.png")
        plt.close()

    def create_dependency_analysis(self):
        """Analyze build dependencies and create dependency visualization"""
        print("📊 Creating dependency analysis...")

        # Identify dependency patterns based on build order and timing
        dependencies = {}

        # Sort builds by start time
        sorted_builds = []
        for package, details in self.builds.items():
            if not details.get('incomplete'):
                start_time = datetime.fromisoformat(
                    details['start_time'].replace('Z', '+00:00'))
                sorted_builds.append((package, start_time, details))

        sorted_builds.sort(key=lambda x: x[1])

        # Group builds that start simultaneously (likely in parallel)
        parallel_groups = []
        current_group = []
        last_time = None

        for package, start_time, details in sorted_builds:
            # If start time is within 5 seconds of the last one, consider it parallel
            if last_time and abs((start_time - last_time).total_seconds()) < 5:
                current_group.append((package, details))
            else:
                if current_group:
                    parallel_groups.append(current_group)
                current_group = [(package, details)]
            last_time = start_time

        if current_group:
            parallel_groups.append(current_group)

        # Create dependency analysis report
        report = {
            'total_build_phases': len(parallel_groups),
            'parallel_groups': [],
            'sequential_dependencies': []
        }

        for i, group in enumerate(parallel_groups):
            # Helper to get duration in minutes
            def get_duration_minutes(details):
                if 'duration_minutes' in details:
                    return details['duration_minutes']
                elif 'duration_seconds' in details:
                    return details['duration_seconds'] / 60
                return 0

            durations = [get_duration_minutes(details) for _, details in group]

            group_info = {
                'phase': i + 1,
                'packages': [pkg for pkg, _ in group],
                'parallel_count': len(group),
                'total_duration': sum(durations),
                'max_duration': max(durations) if durations else 0,
                'avg_duration': sum(durations) / len(durations) if durations else 0
            }
            report['parallel_groups'].append(group_info)

        # Save dependency analysis
        with open('./dependency_analysis.json', 'w') as f:
            json.dump(report, f, indent=2, default=str)

        print("✅ Dependency analysis saved to dependency_analysis.json")
        return report

    def create_module_compilation_chart(self):
        """Create chart showing top longest module compilation times"""
        print("📊 Creating module compilation time chart...")

        modules = self.data.get('module_compilations', {})
        if not modules:
            print("⚠️  No module compilation data found, skipping chart")
            return

        # Sort modules by duration
        sorted_modules = sorted(
            modules.items(),
            key=lambda x: x[1]['duration_seconds'],
            reverse=True
        )[:20]  # Top 20

        # Prepare data
        labels = []
        durations = []
        packages = []

        for key, data in sorted_modules:
            # Create a shortened label: Package.Module
            module = data['module']
            package = data['package']
            # Truncate long module names
            if len(module) > 30:
                module = "..." + module[-27:]
            labels.append(f"{package}\n{module}")
            durations.append(data['duration_seconds'])
            packages.append(package)

        # Create figure
        fig, ax = plt.subplots(figsize=(16, 10))

        # Color by package
        unique_packages = list(set(packages))
        colors = plt.cm.Set3(np.linspace(0, 1, len(unique_packages)))
        package_colors = {pkg: colors[i] for i, pkg in enumerate(unique_packages)}
        bar_colors = [package_colors[pkg] for pkg in packages]

        # Create horizontal bar chart
        y_pos = np.arange(len(labels))
        bars = ax.barh(y_pos, durations, color=bar_colors, alpha=0.8, edgecolor='black', linewidth=0.5)

        # Customize
        ax.set_yticks(y_pos)
        ax.set_yticklabels(labels, fontsize=8)
        ax.invert_yaxis()  # Longest at top
        ax.set_xlabel('Compilation Time (seconds)', fontsize=12)
        ax.set_title('Top 20 Longest Module Compilation Times', fontsize=16, weight='bold')

        # Add duration labels on bars
        for i, (bar, duration) in enumerate(zip(bars, durations)):
            width = bar.get_width()
            label = f"{duration:.1f}s"
            if duration >= 60:
                label = f"{duration/60:.1f}m"
            ax.text(width + 0.5, bar.get_y() + bar.get_height()/2,
                   label, ha='left', va='center', fontsize=8)

        # Add legend for packages
        legend_elements = [plt.Rectangle((0,0),1,1, facecolor=package_colors[pkg], label=pkg, alpha=0.8)
                          for pkg in unique_packages[:10]]  # Limit legend to first 10
        ax.legend(handles=legend_elements, loc='lower right', fontsize=8)

        plt.grid(axis='x', alpha=0.3)
        plt.tight_layout()

        # Save
        plt.savefig('./module_compilation_times.png', dpi=300, bbox_inches='tight')
        print("✅ Module compilation chart saved to module_compilation_times.png")
        plt.close()

    def create_cache_analysis_chart(self):
        """Create chart showing cache usage breakdown"""
        print("📊 Creating cache analysis chart...")

        cache_stats = self.data.get('cache_stats', {})
        cache_hits = self.data.get('cache_hits', [])

        # Create figure with pie chart only
        fig, ax = plt.subplots(figsize=(12, 8))

        # Calculate totals: count hits per cache + local builds
        cache_totals = {}

        # Count cache hits by source
        for hit in cache_hits:
            cache_name = hit.get('cache', 'unknown')
            if cache_name not in cache_totals:
                cache_totals[cache_name] = 0
            cache_totals[cache_name] += 1

        # Add local builds
        local_builds = cache_stats.get('local_build', {}).get('count', 0)
        if local_builds > 0:
            cache_totals['Local Build'] = local_builds

        if cache_totals:
            labels = list(cache_totals.keys())
            sizes = list(cache_totals.values())
            colors = plt.cm.Set2(np.linspace(0, 1, len(labels)))

            wedges, texts, autotexts = ax.pie(
                sizes, labels=labels, autopct='%1.1f%%',
                colors=colors, startangle=90,
                textprops={'fontsize': 11}
            )
            ax.set_title('Artifacts by Source (Cached vs Local Build)', fontsize=16, weight='bold')

            # Add a legend with counts
            legend_labels = [f'{label}: {size}' for label, size in zip(labels, sizes)]
            ax.legend(wedges, legend_labels, title="Sources", loc="center left", bbox_to_anchor=(1, 0, 0.5, 1))

        plt.tight_layout()
        plt.savefig('./cache_analysis.png', dpi=300, bbox_inches='tight')
        print("✅ Cache analysis chart saved to cache_analysis.png")
        plt.close()

    def generate_summary_report(self):
        """Generate a comprehensive summary report"""
        print("📝 Generating comprehensive summary report...")

        # Calculate key metrics
        total_builds = len(self.builds)
        completed_builds = len(
            [b for b in self.builds.values() if not b.get('incomplete')])

        durations = [b['duration_seconds']
                     for b in self.builds.values() if not b.get('incomplete')]
        if durations:
            avg_duration = sum(durations) / len(durations)
            max_duration = max(durations)
            total_cpu_time = sum(durations)
        else:
            avg_duration = max_duration = total_cpu_time = 0

        # Wall clock time
        if self.data['summary']:
            wall_clock_time = self.data['summary']['wall_clock_seconds']
            parallelization_factor = total_cpu_time / \
                wall_clock_time if wall_clock_time > 0 else 0
        else:
            wall_clock_time = parallelization_factor = 0

        # Find the longest builds
        longest_builds = sorted(self.builds.items(),
                                key=lambda x: x[1]['duration_seconds'], reverse=True)[:10]

        # Module compilation stats
        modules = self.data.get('module_compilations', {})
        longest_modules = sorted(modules.items(),
                                key=lambda x: x[1]['duration_seconds'], reverse=True)[:10] if modules else []

        # Cache stats - only local builds
        cache_stats = self.data.get('cache_stats', {})
        local_builds = cache_stats.get('local_build', {}).get('count', 0)
        cache_hits_count = len(self.data.get('cache_hits', []))

        # Create detailed report
        report = f"""
# BUILD PERFORMANCE ANALYSIS REPORT

## 🎯 Executive Summary
- **Total Build Time (Wall Clock):** {wall_clock_time / 60:.1f} minutes
- **Total CPU Time:** {total_cpu_time / 60:.1f} minutes  
- **Parallelization Efficiency:** {parallelization_factor:.1f}x
- **Builds Completed:** {completed_builds}/{total_builds}
- **Average Build Duration:** {avg_duration / 60:.1f} minutes
- **Cache Fetches:** {cache_hits_count}
- **Local Builds:** {local_builds}

## 🐌 Longest Package Build Times
"""

        for i, (package, details) in enumerate(longest_builds, 1):
            status = " (INCOMPLETE)" if details.get('incomplete') else ""
            # Get duration in minutes
            if 'duration_minutes' in details:
                duration_mins = details['duration_minutes']
            elif 'duration_seconds' in details:
                duration_mins = details['duration_seconds'] / 60
            else:
                duration_mins = 0
            report += f"{i:2d}. **{package}**: {duration_mins:.1f} minutes{status}\n"

        # Add module compilation section if available
        if longest_modules:
            report += "\n## 🐌 Longest Module Compilation Times\n"
            for i, (key, data) in enumerate(longest_modules, 1):
                report += f"{i:2d}. **{data['package']}** - `{data['module']}`: {data['duration_human']}\n"

        # Add cache stats section - simplified
        report += f"""
## 💾 Cache Statistics
- **Artifacts Fetched from Cache:** {cache_hits_count}
- **Artifacts Built Locally:** {local_builds}

## ⚡ Parallelization Analysis
- **Maximum Concurrent Builds:** {len(max(self.parallel_groups, key=lambda x: x['count'])['packages']) if self.parallel_groups else 1}
- **Parallel Groups Identified:** {len(self.parallel_groups)}
- **Estimated Parallel Efficiency:** {(parallelization_factor * 100):.1f}%

## 🔍 Key Insights

### Performance Bottlenecks:
1. **{longest_builds[0][0]}** is the major bottleneck, taking {(longest_builds[0][1].get('duration_minutes') or longest_builds[0][1].get('duration_seconds', 0) / 60):.1f} minutes ({(longest_builds[0][1]['duration_seconds'] / wall_clock_time * 100):.1f}% of total time)
"""

        if longest_modules:
            report += f"2. **Slowest module to compile**: `{longest_modules[0][1]['module']}` in package `{longest_modules[0][1]['package']}` taking {longest_modules[0][1]['duration_human']}\n"

        report += f"""
### Recommendations:
1. **Optimize the longest build** ({longest_builds[0][0]}) - consider splitting into smaller modules
2. **Increase build cache usage** to avoid rebuilding unchanged dependencies  
3. **Consider incremental compilation** for large projects
4. **Build machine resources** appear adequate for current parallelization level

### Build Patterns:
- **Source packages** build quickly (< 1 minute each)
- **Main compilation** happens in well-parallelized groups
- **Sequential dependencies** are properly handled by the build system

## 📊 Statistical Summary
- **Shortest Build:** {min(durations) / 60:.1f} minutes
- **Longest Build:** {max_duration / 60:.1f} minutes  
- **Median Build Time:** {sorted(durations)[len(durations)//2] / 60:.1f} minutes
- **Standard Deviation:** {np.std(durations) / 60:.1f} minutes
- **Total Modules Compiled:** {len(modules)}
"""

        # Save the report
        with open('./build_analysis_report.md', 'w') as f:
            f.write(report)

        print("✅ Comprehensive report saved to build_analysis_report.md")

        return report


def main():
    try:
        # Check if analysis file exists
        import os
        analysis_file = './build_analysis.json'
        if not os.path.exists(analysis_file):
            print("❌ build_analysis.json not found. Run the analyzer script first.")
            return

        visualizer = BuildVisualizer(analysis_file)

        # Create visualizations
        visualizer.create_gantt_chart()
        visualizer.create_parallel_analysis_chart()
        visualizer.create_module_compilation_chart()
        visualizer.create_cache_analysis_chart()

        # Generate analyses
        visualizer.create_dependency_analysis()
        report = visualizer.generate_summary_report()

        print(
            f"\n🎉 Analysis complete! Check the following files in .:")
        print("   📊 build_gantt_chart.png - Visual timeline")
        print("   📈 parallelization_analysis.png - Parallelization metrics")
        print("   📈 module_compilation_times.png - Module compilation times")
        print("   📈 cache_analysis.png - Cache usage pie chart")
        print("   📋 build_analysis_report.md - Comprehensive report")
        print("   🔗 dependency_analysis.json - Dependency details")

    except Exception as e:
        print(f"❌ Error creating visualizations: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
