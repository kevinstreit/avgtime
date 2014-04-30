AvgTime
=======

This is a simple program to execute a command a given number of times and report
one or multiple of arithmetic mean, the geometric mean and the median execution
times over all runs of the individual tools.

Usage is as follows:

```
  ./avgtime [Options]

  Options:
      -h --help           print this help menu
      -c --command cmd    Command (including arguments) to execute.
      -n --times n        Execute the command <n> times
      -a --avg            Report the arithmetic meanover all runs.
      -g --geomean        Report the geometric mean over all runs.
      -m --median         Report the median over all runs.
```
