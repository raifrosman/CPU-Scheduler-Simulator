const std = @import("std");

//////////////////////////////////////////////// DATA STRUCTURES /////////////////////////////////////////////////////////

// Process IO struct.
const IOList = struct {
    io_time: i32, // CPU time of IO.
    io_burst: i32, // Burst time in IO wait.
};

// Process struct.
const Process = struct {
    number: usize, // For verbose mode tracking.
    label: u8,
    arrival_time: f32,
    cycle_count: f32, // CPU time for output.
    start_time: f32,
    wait_time: f32,
    finish_time: f32,
    cpu_burst: f32, // CPU time bursting.
    io_flag: bool, // For io check if exist.
    io_list: ArrayList(IOList), // List of IO(s).
    turnaround_time: f32,
    normalized_turnaround_time: f32,
    quantum_burst: i32,
    quantum_current: i32,
};

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

const ArrayList = std.ArrayList; // Array List.

// Configure scheduler type from command line argument input.
fn scheduler_config(arg: []u8) usize {
    var scheduler: usize = 0;

    if (std.mem.eql(u8, arg, "FF")) {
        scheduler = 1;
    } else if (std.mem.eql(u8, arg, "RR")) {
        scheduler = 2;
    } else if (std.mem.eql(u8, arg, "SP")) {
        scheduler = 3;
    } else if (std.mem.eql(u8, arg, "SR")) {
        scheduler = 4;
    } else if (std.mem.eql(u8, arg, "HR")) {
        scheduler = 5;
    } else if (std.mem.eql(u8, arg, "FB")) {
        scheduler = 6;
    } else {
        scheduler = 0; // Invalid.
    }

    return scheduler;
}

// Usage error (Command line arguments invalid).
fn usage() !void {
    // Instructions say to ignore quantum time args, but the test run FF and SP without q.
    std.debug.print("INVALID COMMAND LINE ARGUMENTS\n", .{});
    std.debug.print("Valid Usages:\n", .{});
    std.debug.print("usage1: ./schsim -v -s <FF/SP/SR/HR> <input_file_path> <output_file_path>\n", .{});
    std.debug.print("usage2: ./schsim -v -s <RR/FB> -q <quantum_time>0> <input_file_path> <output_file_path>\n", .{});
    std.process.exit(1);
}

///////////////////////////////////////////////////// MAIN ////////////////////////////////////////////////////////////////
pub fn main() !void {
    ////////////////////////////////////////////// Memory Allocator ///////////////////////////////////////////////////////
    var gpa = std.heap.GeneralPurposeAllocator(.{}){}; // Dynamic memory allocation manager.

    defer {
        const deinit_status = gpa.deinit(); // Deinitialize and free.
        if (deinit_status == .leak) @panic("LEAK"); // Leak error.
    }

    const alloc = gpa.allocator(); // Get allocator handler.

    ///////////////////////////////////////////// Command Line Args ///////////////////////////////////////////////////////
    const args = try std.process.argsAlloc(alloc); // Allocates memory for command-line arguments.
    defer std.process.argsFree(alloc, args); // Free.

    if (args.len > 8) try usage(); // Usage error.

    // Get verbose flag.
    var verbose: bool = false;
    if (std.mem.eql(u8, args[1], "-v")) // If verbose mode is given.
        verbose = true; // Update to true.

    // Get scheduler.
    var scheduler: usize = 0;
    if (std.mem.eql(u8, args[1], "-s")) { // Depends if verbose flag in args.
        scheduler = scheduler_config(args[2]); // Configure scheduler type.
    } else if (std.mem.eql(u8, args[2], "-s")) {
        scheduler = scheduler_config(args[3]);
    }

    if (scheduler == 0) try usage(); // Usage error.

    // Get quantum time.
    var quantum_time: i32 = 0;
    if ((std.mem.eql(u8, args[args.len - 4], "-q"))) { // If quantum time is given.
        quantum_time = try std.fmt.parseInt(i32, args[args.len - 3], 10); // Quantum time value.

        if (quantum_time < 1) try usage(); // Usage error.
    }

    // Get input file.
    const infile = try std.fs.cwd().openFile(args[args.len - 2], .{}); // Open file.
    defer infile.close(); // Close file.

    // Get the file size for allocation size.
    const file_info = try infile.stat();
    const file_size = file_info.size;

    // Read file.
    var buffered_reader = std.io.bufferedReader(infile.reader()); // Read with buffer.
    const reader = buffered_reader.reader(); // Handler.
    const content = try reader.readAllAlloc(alloc, file_size); // Read and write file into memory (content).
    defer alloc.free(content); // Free.

    //////////////////////////////////////////////// Arrival Queue ////////////////////////////////////////////////////////
    var arrival_queue = ArrayList(Process).init(alloc);
    defer arrival_queue.deinit();

    /////////////////////////////////////////////////// Parsing ///////////////////////////////////////////////////////////
    var lines = std.mem.tokenizeAny(u8, content, "\r\n"); // Tokenize/Split every word.

    while (lines.next()) |line| { // Loop each token.
        var tokens = std.mem.tokenizeAny(u8, line, ", "); // Tokenize/Split every word.
        var process: Process = undefined; // Declare a process.

        // Initializes values of Process object.
        process.number = 0;
        process.arrival_time = 0;
        process.cycle_count = 0;
        process.start_time = -1;
        process.wait_time = 0;
        process.finish_time = 0;
        process.cpu_burst = 0;
        process.io_flag = false;
        process.io_list = ArrayList(IOList).init(alloc);
        process.turnaround_time = 0;
        process.normalized_turnaround_time = 0;
        process.quantum_burst = 0;
        process.quantum_current = 0;

        var count: i8 = 1; // Counter for tokens from each line.
        if (line[0] == '"') { // A line indicates a process.
            while (tokens.next()) |token| {
                switch (count) {
                    1 => process.label = token[1], // Get label.
                    2 => process.arrival_time = try std.fmt.parseFloat(f32, token), // Get arrival time.
                    3 => process.cycle_count = try std.fmt.parseFloat(f32, token), // Get cpu cycle time.
                    else => break,
                }
                count += 1;
            }

            process.cpu_burst = process.cycle_count; // Add cycle count to total burst time.

        } else { // A line indicates an IO operation for a process.
            var io: IOList = undefined;

            process = arrival_queue.pop(); // Get the last added process.
            process.io_flag = true; // Showing that a process has IO operation(s).

            while (tokens.next()) |token| {
                switch (count) {
                    1 => io.io_time = try std.fmt.parseInt(i32, token, 10), // Get io time.
                    2 => io.io_burst = try std.fmt.parseInt(i32, token, 10), // Get io burst time.
                    else => break,
                }
                count += 1;
            }

            try process.io_list.append(io); // Add the IO operation to the process's IO list.

        }

        process.number = arrival_queue.items.len + 1; // For verbose mode.
        try arrival_queue.append(process); // Add (or re-add) process to the arrival queue.
    }

    //////////////////////////////////////////////////// SIMULATE /////////////////////////////////////////////////////////
    const finish_queue = try scheduler_simulator(verbose, scheduler, quantum_time, &arrival_queue, arrival_queue.items.len, alloc); // Simulate scheduler.
    defer finish_queue.deinit(); // Free.
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    var mean: [2]f32 = undefined;
    var total_turnaround: f32 = 0;
    var total_normalized: f32 = 0;
    var total_process: f32 = 0;

    for (finish_queue.items) |process| { // For all process.
        total_turnaround += process.turnaround_time; // Compute total turnaround time.
        total_normalized += process.normalized_turnaround_time; // Compute total normalized turnaround time.
        total_process += 1; // Get total number of process in float for division.
    }

    // Compute mean of turnaround time and normalized turnaround time.
    mean[0] = total_turnaround / total_process;
    mean[1] = total_normalized / total_process;

    //////////////////////////////////////////////// OUTPUT TO FILE ///////////////////////////////////////////////////////
    const outfile = try std.fs.cwd().createFile(args[args.len - 1], .{}); // Create an output file.
    defer outfile.close(); // Free.

    try outputToFile(alloc, finish_queue, outfile, &mean); // Print to file.

    // Free ArrayList of IO List.
    for (finish_queue.items) |p| {
        p.io_list.deinit();
    }
}

// Output content to a file.
fn outputToFile(alloc: std.mem.Allocator, finish_queue: ArrayList(Process), outfile: std.fs.File, mean: []f32) !void {
    var bytes = try outfile.write("\"name\", \"arrival time\", \"service time\", \"start time\", \"total wait time\", \"finish time\", \"turnaround time\", \"normalized turnaround\"\n"); // Print header.

    for (finish_queue.items) |process| { // For each process.
        const buffer = try std.fmt.allocPrint(alloc, "\"{c}\", {d}, {d}, {d}, {d}, {d}, {d}, {d:.2}\n", .{ process.label, process.arrival_time, process.cycle_count, process.start_time, process.wait_time, process.finish_time, process.turnaround_time, process.normalized_turnaround_time }); // Buffer for string format.
        defer alloc.free(buffer); // Free.

        bytes += try outfile.write(buffer); // Print to file.
    }

    var c: usize = 0;
    while (c < 2) : (c += 1) {
        const buffer = try std.fmt.allocPrint(alloc, "{d:.2}", .{mean[c]}); // Buffer for string format.
        defer alloc.free(buffer); // Free.

        if (c == 0) { // Mean turnaround time.
            bytes += try outfile.write(buffer); // Print to file.
            bytes += try outfile.write(", "); // Formatting.
        } else if (c == 1) { // Mean normalized turnaround time.
            bytes += try outfile.write(buffer); // Print to file.
            bytes += try outfile.write("\n"); // Formating.
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////// SIMULATOR //////////////////////////////////////////////////////////

// Global variable.
var cpu_clock: f32 = 0;
var quantum_clock: i32 = 0;

// Simulator function.
fn scheduler_simulator(verbose: bool, scheduler: usize, quantum_time: i32, arrival_queue: *ArrayList(Process), num_process: usize, alloc: std.mem.Allocator) !ArrayList(Process) {
    // Initialize queues.
    var ready_queue = ArrayList(Process).init(alloc);
    var cpu = ArrayList(Process).init(alloc);
    var io_wait = ArrayList(Process).init(alloc);
    var finish_queue = ArrayList(Process).init(alloc);

    // Free.
    defer ready_queue.deinit();
    defer cpu.deinit();
    defer io_wait.deinit();

    // Initialize quantum clock.
    quantum_clock = quantum_time;

    // Verbose mode ouput header format.
    if (verbose and cpu_clock == 0) {
        std.debug.print("    ", .{});
        for (arrival_queue.items) |p| {
            std.debug.print("| {c} ", .{p.label});
        }
        std.debug.print("|\n", .{});
    }

    // Start process scheduling.
    while (finish_queue.items.len != num_process) {
        try process_time(&cpu, &ready_queue, &io_wait); // Process time operation.

        try cpu_to_finish(&cpu, &finish_queue); // Move process from cpu to finish queue.

        const update = try arrival_to_ready(scheduler, quantum_time, arrival_queue, &ready_queue); // Move process from arrival to ready queue.

        try cpu_to_io(&cpu, &io_wait); // Move process from cpu to io wait.

        if (ready_queue.items.len != 0 and cpu.items.len != 0) {
            if (scheduler == 2 and quantum_clock <= 0) { // For RR.
                try preempt(scheduler, quantum_time, &ready_queue, &cpu, &finish_queue); // Preempt process is needed.

            } else if (scheduler == 4) { // For SR.
                try preempt(scheduler, quantum_time, &ready_queue, &cpu, &finish_queue); // Preempt process is needed.

            } else if (scheduler == 6 and (cpu.items[0].quantum_burst <= 0 or update == true)) { // For FB.
                try preempt(scheduler, quantum_time, &ready_queue, &cpu, &finish_queue); // Preempt process is needed.

            }
        }

        if (scheduler == 6 and cpu.items.len != 0 and cpu.items[0].quantum_burst <= 0 and ready_queue.items.len == 0)
            try preempt(scheduler, quantum_time, &ready_queue, &cpu, &finish_queue); // Preempt process is needed.

        try io_to_ready(&io_wait, &ready_queue); // Move process from cpu to io wait.

        try ready_to_cpu(scheduler, quantum_time, &ready_queue, &cpu, &finish_queue); // Ready queue to CPU operation, if empty (CPU select).

        if (verbose) try verbose_output(num_process, arrival_queue, &ready_queue, &cpu, &io_wait, &finish_queue); // If verbose mode flag is true.

        tick(); // Increment clock cycle.
    }

    return finish_queue;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////// SCHEDULER FUNCTION ////////////////////////////////////////////////////////

// Time operation on CPU burst time, io time, wait time, io burst, & quantum time.
fn process_time(cpu: *ArrayList(Process), ready_queue: *ArrayList(Process), io_wait: *ArrayList(Process)) !void {
    // Decrease burst time of the process in CPU.
    if (cpu.items.len != 0) { // If cpu is busy.
        cpu.items[0].cpu_burst -= 1; // Decrease cpu burst time.
        quantum_clock -= 1; // Decrease quantum time.

        // Decrease io time (if exist) of the process in CPU.
        if (cpu.items[0].io_flag == true and cpu.items[0].io_list.items.len != 0) // If the process have io.
            cpu.items[0].io_list.items[0].io_time -= 1; // Decrease io time of current io.

        cpu.items[0].quantum_burst -= 1; // For FB schedule.
    }

    // Increase wait time of processes in ready queue.
    if (ready_queue.items.len != 0) { // If ready queue is not empty.
        for (ready_queue.items) |*process| { // For each process.
            process.wait_time += 1; // Increment wait time.
        }
    }

    // Increase wait time & decrease io burst of processes in io wait.
    if (io_wait.items.len != 0) { // If io wait is not empty.
        for (io_wait.items) |*process| { // For each process.
            process.wait_time += 1; // Increment  wait time.
            process.io_list.items[0].io_burst -= 1; // Decrement every process io burst at the same time?
        }
    }
}

// Move the process from CPU to finish queue operation.
fn cpu_to_finish(cpu: *ArrayList(Process), finish_queue: *ArrayList(Process)) !void {
    if (cpu.items.len != 0 and cpu.items[0].cpu_burst <= 0) { // Check CPU status, and process burst time.
        cpu.items[0].finish_time = cpu_clock; // Clock finish time.
        cpu.items[0].turnaround_time = cpu.items[0].finish_time - cpu.items[0].arrival_time; // Compute turnaround time.
        cpu.items[0].normalized_turnaround_time = cpu.items[0].turnaround_time / cpu.items[0].cycle_count; // Compute normalized turnaround time.

        try finish_queue.append(cpu.pop()); // Move to finish queue.
    }
}

// Move the process from arrival queue to ready queue operation.
fn arrival_to_ready(scheduler: usize, quantum_time: i32, arrival_queue: *ArrayList(Process), ready_queue: *ArrayList(Process)) !bool {
    if (arrival_queue.items.len != 0 and arrival_queue.items[0].arrival_time == cpu_clock) { // Check process in arrival queue (arrival time = clock cycle).
        if (scheduler == 6) { // Initialize given quantum time for FB scheduling.
            arrival_queue.items[0].quantum_current = quantum_time;
            arrival_queue.items[0].quantum_burst = quantum_time;
        }

        try ready_queue.append(arrival_queue.orderedRemove(0)); // Move to ready queue.

        return true; // For FB.
    }
    return false; // For FB.
}

// Move process from CPU to IO wait.
fn cpu_to_io(cpu: *ArrayList(Process), io_wait: *ArrayList(Process)) !void {
    if (cpu.items.len != 0) { // If cpu is busy.
        if (cpu.items[0].io_flag == true) { // If process has IO.
            if (cpu.items[0].io_list.items.len != 0) { // If process still has uncompleted IO.
                if (cpu.items[0].io_list.items[0].io_time == 0) { // If process current IO time reach zero.
                    try io_wait.append(cpu.pop()); // Move to IO wait.
                }
            }
        }
    }
}

// Preempt process from CPU.
fn preempt(scheduler: usize, quantum_time: i32, ready_queue: *ArrayList(Process), cpu: *ArrayList(Process), finish_queue: *ArrayList(Process)) !void {
    // Check scheduler type.
    if (scheduler == 2) { // IF RR scheduler and quantum clock reach zero.
        try rr_select(ready_queue, cpu); // Call RR select.

    } else if (scheduler == 4) { // If SR scheduler.
        try sr_select(ready_queue, cpu); // Call SR select.

    } else if (scheduler == 6) { // If FB.
        try fb_select(ready_queue, cpu); // Call FB select.
    } else return; // Exit function.

    if (cpu.items[0].start_time < 0) { // Check if process start time have not clocked.
        cpu.items[0].start_time = cpu_clock; // Clock the start time (current cpu clock).
    }

    if (cpu.items.len != 0 and cpu.items[0].cpu_burst <= 0) { // Check CPU, and process burst time.
        cpu.items[0].finish_time = cpu_clock; // Clock finish time.
        try finish_queue.append(cpu.pop()); // Move the process from CPU to finish queue.
    }

    quantum_clock = quantum_time; // Reset quantum time.
}

// Move process from IO wait to ready queue.
fn io_to_ready(io_wait: *ArrayList(Process), ready_queue: *ArrayList(Process)) !void {
    if (io_wait.items.len != 0) {
        var i: usize = io_wait.items.len; // Processes index.

        while (i > 0) : (i -= 1) { // Decremental check.
            if (io_wait.items[i - 1].io_list.items[0].io_burst <= 0) { // Check IO wait for completed IO process (io burst = 0).
                _ = io_wait.items[i - 1].io_list.orderedRemove(0); // Remove completed io.
                try ready_queue.append(io_wait.orderedRemove(i - 1)); // Move to ready queue.
            }
        }
    }
}

// Move process from ready queue to CPU.
fn ready_to_cpu(scheduler: usize, quantum_time: i32, ready_queue: *ArrayList(Process), cpu: *ArrayList(Process), finish_queue: *ArrayList(Process)) !void {
    outer: while (cpu.items.len == 0 and ready_queue.items.len != 0) { // Check cpu and ready queue spaces.
        // Scheduler check.
        switch (scheduler) {
            1 => try ff_select(ready_queue, cpu), // FF
            2 => try rr_select(ready_queue, cpu), // RR
            3 => try sp_select(ready_queue, cpu), // SP
            4 => try sr_select(ready_queue, cpu), // SR
            5 => try hr_select(ready_queue, cpu), // HR
            6 => try fb_select(ready_queue, cpu), // FB
            else => return,
        }

        if (cpu.items.len != 0 and cpu.items[0].cpu_burst <= 0) { // Check CPU, and process burst time.
            cpu.items[0].finish_time = cpu_clock; // Clock finish time.
            try finish_queue.append(cpu.pop()); // Move the process from CPU to finish queue.
            continue :outer;
        }

        if (cpu.items.len != 0 and cpu.items[0].start_time < 0) { // Check if process start time have not clocked.
            cpu.items[0].start_time = cpu_clock; // Clock the start time (current cpu clock).
        }

        quantum_clock = quantum_time; // Reset quantum clock.
    }
}

// Increment CPU clock.
fn tick() void {
    cpu_clock += 1;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////// CPU SELECT FUNCTION ////////////////////////////////////////////////////////

// FCFS Select: Select the process in ready queue that arrived first (arrival_time).
fn ff_select(ready_queue: *ArrayList(Process), cpu: *ArrayList(Process)) !void {
    var selected_index: usize = 0; // Default selected process index.
    var least_arrival_time: f32 = ready_queue.items[0].arrival_time; // Default first arrival time.

    var i: usize = 0;
    for (ready_queue.items) |process| { // Iterate each process.
        if (process.arrival_time < least_arrival_time) { // If current process arrived first.
            least_arrival_time = process.arrival_time; // Update.
            selected_index = i; // Update new index of the selected process.
        }
        i += 1; // Increment counter.
    }

    try cpu.append(ready_queue.orderedRemove(selected_index)); // Move process into the cpu.
}

// RR Select: Select the process in ready queue in FIFO order.
fn rr_select(ready_queue: *ArrayList(Process), cpu: *ArrayList(Process)) !void {
    if (cpu.items.len == 0 and ready_queue.items.len != 0) { // If cpu is idle.
        try cpu.append(ready_queue.orderedRemove(0)); // Move process into the cpu.

    } else if (cpu.items.len != 0 and ready_queue.items.len != 0) { // If cpu is busy (For preempt).
        try ready_queue.append(cpu.pop()); // Preempt current process.
        try cpu.append(ready_queue.orderedRemove(0)); // Move selected process into the cpu.

    }
}

// SJF Select: Select the process in ready queue with least cycle/process time..
fn sp_select(ready_queue: *ArrayList(Process), cpu: *ArrayList(Process)) !void {
    var selected_index: usize = 0; // Default selected process index.
    var least_process_time: f32 = ready_queue.items[0].cycle_count; // Default least cycle time.

    var i: usize = 0;
    for (ready_queue.items) |process| { // Iterate each process.
        if (process.cycle_count < least_process_time) { // If current have less cycle time.
            least_process_time = process.cycle_count; // Update.
            selected_index = i; // Update new index of the selected process.
        }

        i += 1; // Increment counter.
    }

    try cpu.append(ready_queue.orderedRemove(selected_index)); // Move selected process into the cpu.
}

// SRT Select: Select the process in ready queue with shorter burst time left (CPU burst).
fn sr_select(ready_queue: *ArrayList(Process), cpu: *ArrayList(Process)) !void {
    if (cpu.items.len == 0 and ready_queue.items.len != 0) { // If cpu is idle.
        var selected_index: usize = 0; // Default selected process index.
        var least_burst_time: f32 = ready_queue.items[0].cpu_burst; // Default least burst time.

        var i: usize = 0;
        for (ready_queue.items) |process| { // Iterate each process.
            if (process.cpu_burst < least_burst_time) { // If current process have less burst time.
                least_burst_time = process.cpu_burst; // Update.
                selected_index = i; // Update new index of the selected process.
            }

            i += 1; // Increment counter.
        }

        try cpu.append(ready_queue.orderedRemove(selected_index)); // Move selected process into the cpu.

    } else if (cpu.items.len != 0 and ready_queue.items.len != 0) { // If cpu is busy (Preempt).
        // Compare cpu process with ready queue processes for shorter cpu burst left.
        var selected_index: usize = ready_queue.items.len; // Default selected process index.
        var least_burst_time: f32 = cpu.items[0].cpu_burst; // Default least burst time of cpu process.

        var count: usize = 0;
        for (ready_queue.items) |process| { // Iterate each process.
            if (process.cpu_burst < least_burst_time) { // If ready queue process have less burst time.
                least_burst_time = process.cpu_burst; // Update.
                selected_index = count; // Update new index of the selected process.
            }

            count += 1; // Increment counter.
        }

        if (selected_index != ready_queue.items.len) { // If shorter burst time process exist.
            try ready_queue.append(cpu.pop()); // Preempt current process.
            try cpu.append(ready_queue.orderedRemove(selected_index)); // Move selected process into the cpu.
        }
    }
}

// HRRN Select: Select the process in ready queue with highest response ratio (Wait time & Cycle time).
fn hr_select(ready_queue: *ArrayList(Process), cpu: *ArrayList(Process)) !void {
    var selected_index: usize = 0; // Default selected process index.
    var highest_response: f32 = (ready_queue.items[0].wait_time + ready_queue.items[0].cycle_count) / ready_queue.items[0].cycle_count; // HRRN formula.

    var i: usize = 0;
    for (ready_queue.items) |process| { // Iterate each process.
        const current_process_ratio: f32 = (process.wait_time + process.cycle_count) / process.cycle_count; // HRRN formula.
        if (current_process_ratio > highest_response) { // If current ready queue process has higher response ratio.
            highest_response = current_process_ratio; // Update.
            selected_index = i; // Update new index of the selected process.
        }

        i += 1; // Increment counter.
    }

    try cpu.append(ready_queue.orderedRemove(selected_index)); // Move selected process into the cpu.
}

// I tried implementing MLF Feedback from zybook. I don't know specifically which type of feedback
// the instruction wants, but the only feedback scheduling I found is MLF from zybook. But, obviously my
// output is not the same with the tests for FB scheduling. Like if it is true the FB for this assignment is
// MLF, why process E from input1 FB-q2 terminates at time 14 when it only has 2 service time and starts at time 8.
// With q = 2, shouldn't it complete within it? Then, for process Y from input2 FB-q2, why Y starts at 2 when
// X used q = 2 until time 2, so Y should start at time 3. If X burst end at 2 at Y starts at 2 too, then Y burst
// will start at 2 instead of 3. This also means that the verbose mode would have two hastags on line 2.
// I could say the same for another feedback type I knew from chatgpt where it uses same quantum time but
// different priority level. For q = 2, Y would still start at 3 since X is using 2 quantum time for 1 and 2.
// Since X came from the first priority same of Y and this level, Y can't preempt X yet. Anyway, I tried my best to
// implement the MLF based on what I understand :| I'm not sure if it's right cause there are some part I'm still not
// sure about MLF from zybook like if higher priority process preempts a lower one, would the lower one
// resume the quantum burst or reset when it reenters the cpu. Actually, I planned to submit without implementing
// the FB but I just felt like trying :)

// MLF Select: Used priority queues with doubled quantum time each level.
fn fb_select(ready_queue: *ArrayList(Process), cpu: *ArrayList(Process)) !void {
    if (cpu.items.len == 0 and ready_queue.items.len != 0) { // If cpu is idle.
        var selected_index: usize = 0; // Default selected process index.
        var least_quantum_time: i32 = ready_queue.items[0].quantum_current; // Default least quantum time.

        var i: usize = 0;
        for (ready_queue.items) |process| { // Iterate each process.
            if (process.quantum_current < least_quantum_time) { // If current process have less quantum time (higher priority).
                least_quantum_time = process.quantum_current; // Update.
                selected_index = i; // Update new index of the selected process.
            }

            i += 1; // Increment counter.
        }

        try cpu.append(ready_queue.orderedRemove(selected_index)); // Move process into the cpu.

    } else if (cpu.items.len != 0 and ready_queue.items.len != 0) { // If cpu is busy (For preempt).
        var selected_index: usize = undefined;
        var least_quantum_time: i32 = undefined;

        if (cpu.items[0].quantum_burst <= 0) { // If burst is zero.
            selected_index = 0; // Default least burst time in ready queue.
            least_quantum_time = ready_queue.items[0].quantum_current; // Find least in ready queue.

        } else { // If burst is not zero during preemption.
            selected_index = ready_queue.items.len; // Default least burst time in cpu.
            least_quantum_time = cpu.items[0].quantum_current; // Compare cpu with ready queue processes.
        }

        var i: usize = 0;
        for (ready_queue.items) |process| { // Iterate each process.
            if (process.quantum_current < least_quantum_time) { // If current process have less burst time.
                least_quantum_time = process.quantum_current; // Update.
                selected_index = i; // Update new index of the selected process.
            }

            i += 1; // Increment counter.
        }

        if (cpu.items[0].quantum_burst <= 0) { // If process in cpu used up quantum time.
            cpu.items[0].quantum_current *= 2; // Increase quantum time (decrease priority).
            cpu.items[0].quantum_burst = cpu.items[0].quantum_current; // Update quantum burst time.

            if (selected_index != ready_queue.items.len) { // If shorter burst time (higher priority) process exist.
                try ready_queue.append(cpu.pop()); // Preempt current process.
                try cpu.append(ready_queue.orderedRemove(selected_index)); // Move selected process into the cpu.
            } else {
                try ready_queue.append(cpu.pop()); // Preempt current process.
                try cpu.append(ready_queue.orderedRemove(0)); // Move selected process into the cpu.
            }
        } else {
            if (selected_index != ready_queue.items.len) { // If shorter burst time (higher priority) process exist.
                // cpu.items[0].quantum_burst = cpu.items[0].quantum_current; // Reset burst time?
                try ready_queue.append(cpu.pop()); // Preempt current process.
                try cpu.append(ready_queue.orderedRemove(selected_index)); // Move selected process into the cpu.
            }
        }
    } else if (cpu.items.len != 0 and ready_queue.items.len == 0) { // If ready queue if empty.
        if (cpu.items[0].quantum_burst <= 0) { // But process in cpu used up quantum time.
            cpu.items[0].quantum_current *= 2; // Increase quantum time (decrease priority).
            cpu.items[0].quantum_burst = cpu.items[0].quantum_current; // Update quantum burst time.

            try ready_queue.append(cpu.pop()); // Preempt from cpu.
            try cpu.append(ready_queue.orderedRemove(0)); // Move back into cpu.
        }
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Output for verbose mode.
fn verbose_output(num_process: usize, arrival_queue: *ArrayList(Process), ready_queue: *ArrayList(Process), cpu: *ArrayList(Process), io_wait: *ArrayList(Process), finish_queue: *ArrayList(Process)) !void {
    if (cpu_clock < 10) {
        std.debug.print("  {d}:", .{cpu_clock});
    } else if (cpu_clock >= 10 and cpu_clock < 100) {
        std.debug.print(" {d}:", .{cpu_clock});
    } else std.debug.print("{d}:", .{cpu_clock});

    if (cpu.items.len != 0) {
        var c: usize = 0;
        while (c < num_process) : (c += 1) {
            if (c == cpu.items[0].number - 1) {
                std.debug.print("  # ", .{});
            } else std.debug.print("    ", .{});
        }
    } else {
        var c: usize = 0;
        while (c < num_process) : (c += 1) {
            std.debug.print("    ", .{});
        }
    }
    std.debug.print("|", .{});

    if (arrival_queue.items.len != 0) {
        std.debug.print("arrival = {{", .{});
        var c: usize = 1;
        for (arrival_queue.items) |p| {
            std.debug.print("{c}", .{p.label});
            if (c != arrival_queue.items.len) std.debug.print(", ", .{});
            c += 1;
        }
        std.debug.print("}} ", .{});
    }

    if (ready_queue.items.len != 0) {
        std.debug.print("ready = {{", .{});
        var c: usize = 1;
        for (ready_queue.items) |p| {
            std.debug.print("{c}", .{p.label});
            if (c != ready_queue.items.len) std.debug.print(", ", .{});
            c += 1;
        }
        std.debug.print("}} ", .{});
    }

    if (io_wait.items.len != 0) {
        std.debug.print("io = {{", .{});
        var c: usize = 1;
        for (io_wait.items) |p| {
            std.debug.print("{c}", .{p.label});
            if (c != io_wait.items.len) std.debug.print(", ", .{});
            c += 1;
        }
        std.debug.print("}} ", .{});
    }

    if (finish_queue.items.len != 0) {
        std.debug.print("finish = {{", .{});
        var c: usize = 1;
        for (finish_queue.items) |p| {
            std.debug.print("{c}", .{p.label});
            if (c != finish_queue.items.len) std.debug.print(", ", .{});
            c += 1;
        }
        std.debug.print("}} ", .{});
    }

    std.debug.print("\n", .{});
}
