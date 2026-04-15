# SCALE project ACT-UP model

This encapsulates the ACT-UP model for the SCALE project.
The model is exposed as a TCP server.
The sender of data should open a TCP connection to the model, and send line delimited JSON through
this TCP connection as UTF-8 text, one request per line, and read line delimited JSON back, again one
response per line. The connection need not be closed between requests, but can be if desired.
By default the connection should be through port 21952, but this can easily be changed, if desired, as
described below.

The model is available in three forms:

* a running server on Mneme, described below

* as a Docker image, discussed further below

* or simply as the code in this repo

Here's an example of running the model, using the Mneme-hosted server. Note that in this repo
is a piece of example JSON, `input-sample.json`, which we will pass to the server using netcat.

    nc mneme.lan.cmu.edu 21952 < input-sample.json

The input-sample.json file contains four input things, one per line, and netcat should print four lines of response as follows:

    {"done":"init"}
    {"utility":"=utility","decision":"=decision","done":"done"}
    {"done":"step"}
    {"done":"step"}


**WARNING**. This is not currently constructed to support multiple, concurrent queries simultaneously. The plumbing could easily be so configured, but the actual model code is not re-entrant, and would have to be modified to support such use.

## Running this code

If you don't want to install things on your machine you can also use a Docker container to run it, as described near the end of this document.
Note that when using the Docker version the various process files must be added to the `processes/` directory before building the container from the Dockerfile.

To run this a suitable Common Lisp implementation (SBCL is recommended) and QuickLisp must be installed.
Installing SBCL is easy on a variety of OSes, so long as a pre-built binary is used.
Lisp and QuickLisp only need to be installed once.

For installing SBCL see [https://www.sbcl.org/getting.html](https://www.sbcl.org/getting.html). It's pretty easy, but
on Linux I (dfm) should be able to help, and probably Macintosh, too, though I've not done it there myself
in many years; for Windows, though, I'm unlikely to be much help, never using it myself.

Once you have a Lisp implementation installed you need to install QuickLisp, which is near trivial.
See [https://www.quicklisp.org/beta/](https://www.quicklisp.org/beta/) for details (don't worry that
it says "beta," it's been around and stable for a very long time). If necessary, I can easily help.

Once you've got a QuickLisp enabled Lisp installed to run this stuff (on Linux, Macintosh, or any other UNIX-like OS) just do

    ./run.sh

Once it prints a line like

    <INFO> [12:16:43] scale-act-up-interface - Starting SCALE model listener on port 21952

you should be ready to go; note that it will take a few seconds to get itself ready to listen for connections.
To make it more loquacious, spitting out lots of debugging status and the like, instead run

    ./debug.sh

In Windows these shell scripts won't work, and you'll have to make a suitable replacement. I doubt it would be difficult, but I'm clueless about all things Windows.

Also note that these scripts assume a machine with at least 24GB of physical memory. If running on a smaller machine you may want to decrease the maximum dynamic size requested in them.

To kill the thing just use control-C; it should come down reasonably gracefully if you've used SBCL, perhaps with a little less grace in other Lisp implementations.

If you want to use a different port just supply the number as the sole argument to `run.sh` or `debug.sh`; for example, to use port 9999

    ./run.sh 9999



## Error handling

When there is an error in the interface code
it will simply attempt to return a JSON string describing it. For example,

    (dfm) dfm@carlisle:~/w/scale/act-up-interface$ echo '{"ill-formated": "JSON"]' | nc mneme.lan.cmu.edu 21952

prints

    Error: Expected a `,' separator or `}' in Object on JSON input but found `]' [in #<dynamic-extent STRING-INPUT-STREAM (unavailable) from "{\"ill-fo..."> at position 24]


## Docker container

***Note*** The Docker container was carefully tested when this was initially written. Since then it has been updated as necessary, but not well tested. I **should** still
work, but *caveat emptor.*

If you don't want to install SBCL and so on on a local machine it is also possible to build and run a Docker container, defined by
the `Dockerfile` in the repo. For example,

    (pyibl) dfm@carlisle:~/w/scale/act-up-interface$ docker build -t scale .
    [+] Building 0.0s (0/1)                                                                                                                                                       docker:default
    ... [lots of Docker spoor elided here] ...
     => exporting to image                                                                                                                                                                  0.0s
     => => exporting layers                                                                                                                                                                 0.0s
     => => writing image sha256:12f15be8d79f926b56467cd6aa9f1ba804e9bdf9e426ddac577c05c309848ea2                                                                                            0.0s
     => => naming to docker.io/library/scale                                                                                                                                                0.0s

Then run

    (pyibl) dfm@carlisle:~/w/scale/act-up-interface$ sudo docker run -p 21952:21952 scale
    This is SBCL 2.5.3, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.

    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.
      <INFO> [19:51:40] scale-act-up-interface - Starting SCALE model listener on port 21952

And in a different shell,

    (dfm) dfm@carlisle:~/w/scale/act-up-interface$ cat input-sample.json | nc localhost 21952 >/tmp/output.json

If you are running Windows, netcat can be installed [HERE](https://nmap.org/ncat/).
When installed, ensure nmap is in environmental variables PATH.

Restart Bash and check version.
```bash
which ncat
ncat --version
```

Output should be similar to:

```text
/c/Program Files (x86)/Nmap/ncat
Ncat: Version 7.98 ( https://nmap.org/ncat )
```
You can then run the script, which will use the example outlined above (input-sample.json -> output.json) to test the TCP connection from Docker.
```bash
./test-docker.sh
```
