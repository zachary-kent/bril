import csv
from collections import defaultdict

def calculate_overhead(filename):
    # Create a dictionary to store baseline and ssa values for each benchmark
    data = defaultdict(dict)

    min_overhead = 10000
    max_overhead = -10000
    # Read CSV file
    with open(filename, newline='', encoding='utf-8') as csvfile:
        reader = csv.DictReader(csvfile)
        for row in reader:
            benchmark = row['benchmark']
            run = row['run']
            result = row['result']
            data[benchmark][run] = result
            try:
                min_overhead = min(int(result), min_overhead)
                max_overhead = max(int(result), max_overhead)
            except ValueError:
                pass

    # Find the longest benchmark name for formatting
    max_benchmark_length = max(len(benchmark) for benchmark in data.keys())    
    # Set a fixed width for the percentage overhead column
    overhead_col_width = 15
    overhead_bar_col_width = 61 #2 + int(max_overhead + 10) # - min_overhead)

    # Print markdown table header
    print(f"| {'Bril Benchmark Name'.ljust(max_benchmark_length)} | {'% Overhead'.ljust(overhead_col_width)} | {'% Overhead Bar'.ljust(overhead_bar_col_width)} |")
    print(f"|{'-' * (max_benchmark_length+2)}|{'-' * (overhead_col_width+2)}|{'-' * (overhead_bar_col_width+2)}|")

    # Calculate and print percentage overhead for each benchmark
    for benchmark, values in data.items():
        baseline = values['baseline']
        ssa = values['ssa']
        
        status_messages = ["timeout", "missing", "incorrect"]
        # Check if either value is "timeout" and handle it accordingly
        if baseline in status_messages or ssa in status_messages:
            message = "problem"
            if baseline in status_messages and ssa in status_messages:
                message = "bas&ssa problem"
            else:
                if ssa in status_messages:
                    message = "ssa " + ssa
                if baseline in status_messages:
                    message = "bas " + baseline
            print(f"| {benchmark.ljust(max_benchmark_length)} | {message.ljust(overhead_col_width)} | {'no bar'.ljust(overhead_bar_col_width)} |")
        else:
            # Convert to int for calculations if not "timeout"
            baseline = int(baseline)
            ssa = int(ssa)
            overhead = ((ssa / baseline) - 1) * 100
            bar = " "
            lpad = 10
            ppt = 1
            if overhead < 0.00001:
                if overhead > -0.00001:
                    bar = f"{' '*lpad}" + "0"
                else:  # negative overhead
                    bar = f"{'-'*(-int(overhead/ppt))}" + "0"
                    bar = bar.rjust(lpad+1)
            else:  # positive overhead
                bar = f"{' '*lpad}" + "0" + f"{'+'*(int(overhead/ppt))}"
            print(f"| {benchmark.ljust(max_benchmark_length)} | {f'{overhead:.2f}%'.ljust(overhead_col_width)} | {bar.ljust(overhead_bar_col_width)} |")

# Usage:
# Replace 'ssa_overhead.csv' with the path to your CSV file
calculate_overhead('ssa_overhead.csv')

