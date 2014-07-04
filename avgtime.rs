/*
 * Author: Kevin Streit <streit@kevinstreit.de>
 * Date:   Wed Apr 30 10:54:34 CEST 2014
 *
 * This is a simple "script" to execute a set of given command n times
 * (for a given n) and compute average, geomean and median execution times.
 * The script is mainly used for benchmarking purposes.
 */

extern crate time;
extern crate getopts;
extern crate core;
extern crate collections;

use getopts::{optopt,optmulti,optflag,getopts,usage,OptGroup};
use std::io::process::Command;
use std::os;
use collections::string::String;

fn elapsed_musec(from: time::Timespec, to: time::Timespec) -> i64 {
  ((to.sec - from.sec) as i64) * 1000000 + ((to.nsec - from.nsec) as i64 / 1000)
}

struct ExecResult {
  cleanExecs: u64,

  avg_musec: u64,
  geomean_musec: u64,
  median_musec: u64
}

fn execute_command(cmd: &str, args: &[String], n: u64, checkRetCode: bool) -> ExecResult {
  let mut num_successfull_execs = 0;

  let mut sum_musecs = 0u64;
  let mut prod_musecs = 1f64;
  let mut times : Vec<u64> = Vec::new();

  for i in range(0, n) {
    let beginning = time::get_time();
    match Command::new(cmd).args(args).status() {
      Ok(retCode) => {
        let success = !checkRetCode || retCode.success();

        if success {
          let end = time::get_time();

          let musecs = elapsed_musec(beginning, end) as u64;
          sum_musecs += musecs;
          prod_musecs *= musecs as f64;
          times.push(musecs);
          num_successfull_execs+=1;

          println!("    [{:4} of {} ]: {}µs", i+1, n, musecs);
        } else {
          println!("    [{:4} of {} ]: EXECUTION TERMINATED ABNORMALY ({})", i+1, n, retCode);
        }
      },
      Err(_) => {
        println!("    [{:4} of {} ]: ERROR EXECUTING COMMAND", i+1, n);
      }
    };
  }

  if num_successfull_execs < 1 {
    return ExecResult {
      cleanExecs:    0,
      avg_musec:     0,
      geomean_musec: 0,
      median_musec:  0
    }
  }

  // geomean is nth root of the value products
  let one_by_n = 1 as f64 / num_successfull_execs as f64;
  let geomean = prod_musecs.powf(one_by_n) as u64;

  // Sort to get the median...
  times.sort();
  let medElem : uint = num_successfull_execs as uint / 2;

  ExecResult {
    cleanExecs:    num_successfull_execs,
    avg_musec:     sum_musecs / num_successfull_execs, 
    geomean_musec: geomean, 
    median_musec:  *times.get(medElem)
  }
}

fn print_usage(program: &str, _opts: &[OptGroup]) {
    println!("usage: {} [Options]\n{}", program, usage("", _opts));
}

fn main() {
  let args = os::args();
  let program = args.get(0).clone();

  let opts = [
    optflag ("h", "help",         "print this help menu"),
    optmulti("c", "command",      "Command (including arguments) to execute.", "<cmd>"),
    optflag ("e", "chk-ret-code", "Check the exit code of the programs and take only runs into the measurements that exited cleanly."),
    optopt  ("n", "times",        "Execute the command <n> times",             "<n>"),
    optflag ("a", "avg",          "Report the arithmetic meanover all runs."),
    optflag ("g", "geomean",      "Report the geometric mean over all runs."),
    optflag ("m", "median",       "Report the median over all runs."),
    //optflag ("d", "diff",         "Report the differences between the given commands over all runs.")
  ];

  let matches = match getopts(args.tail(), opts) {
    Ok(m) => { m }
    Err(f) => { fail!(f.to_str()) }
  };

  if matches.opt_present("h") {
    print_usage(program.as_slice(), opts);
    return;
  }

  let n : u64 = match matches.opt_str("n") {
    Some(n_str) => { from_str(n_str.as_slice()).expect("Illegal number format!") }
    None        => { 1 }
  };

  let checkRetCode = matches.opt_present("e");
  let avg = matches.opt_present("a");
  let geomean = matches.opt_present("g");
  let median = matches.opt_present("m");
  // let diff = matches.opt_present("d");

  for cmdstr in matches.opt_strs("c").iter() {
    let mut cmdstrs = cmdstr.as_slice().words();
    let cmd = cmdstrs.next().expect("Empty command string!");
    let argsv : Vec<String> = cmdstrs.map(|s| {s.to_string()}).collect();
    let args = argsv.as_slice();

    println!("==============================");
    println!(" => Executing \"{}\"", cmdstr);
    println!("------------------------------");
    let exec = execute_command(cmd.clone(), args, n, checkRetCode);

    if avg || geomean {
      println!("------------------------------");
      if checkRetCode {
        let runs_str = format!("{} of {}", exec.cleanExecs, n);
        println!(" clean execs: {:>14}", runs_str);
      }
      if avg {
        println!("     average: {:12}µs", exec.avg_musec);
      }
      if geomean {
        println!("     geomean: {:12}µs", exec.geomean_musec);
      }
      if median {
        println!("      median: {:12}µs", exec.median_musec);
      }
    }
    println!("==============================");
    println!("");
  }
}
