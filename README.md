# Usage:

```shell-session
$ git clone git@github.com:zapashcanon/test_comp_runner.git
$ cd test_comp_runner
$ git submodule update --init --depth 1
$ dune exec -- src/runner.exe
```

You can additionally specify a timeout and a limited number of files to run:

```shell-session
$ dune exec -- src/runner.exe 5 1000 # timeout of 5 seconds and limit of 1000 files
```

(It's actually not really a number of files, but it's quite close)

# Generate the report:

```shell-session
$ dune exec -- src/report.exe src/stdout # this is an example of an old run, use the correct file instead
```
