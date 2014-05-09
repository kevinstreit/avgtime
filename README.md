AvgTime
=======

This is a simple program to execute a command a given number of times and report
one or multiple of arithmetic mean, the geometric mean and the median execution
times over all runs of the individual tools.

Usage is as follows:

```
Usage: avgtime [ OPTION... ]
  -h         --help                Show this help
  -c <cmd>   --command=<cmd>       Command (including arguments) to execute.
  -t <cmd>   --cmd-template=<cmd>  Partial command (possibly including arguments) to execute.
                                   The following options (given via '-o') will be combined with this
                                   template to form a command.
  -o <args>  --options=<args>      Command arguments to combine with the last command template (given via '-t').
  -v         --verbose             Print execution times for all command executions.
  -e         --chk-ret-code        Check the exit code of the programs and take only runs into the measurements
                                   that exited cleanly.
  -i         --interleaved         Execute the instances of individual commands interleaved (instead of one
                                   command after the other).
  -n <n>     --times=<n>           Execute the command <n> times (default is 10)
  -a         --avg                 Report the arithmetic mean over all runs of each command.
  -g         --geomean             Report the geometric mean over all runs of each command.
  -m         --median              Report the median over all runs of each command.
  -d         --diff                Report the difference in execution times between individual commands.
```
