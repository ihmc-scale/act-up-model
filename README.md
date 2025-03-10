# SCALE project interface between the reasoner and the ACT-UP model

This is a preliminary pass at an interface between the reasoner and the ACT-UP model for the SCALE project.
It assumes the reasoner will pass data to the ACT-UP model, and synchronously read a return value from it.
In particular, the reasoner should open a TCP connection to the model, and send line delimited JSON through
this TCP connection as UTF-8 text, one request per line, and read line delimited JSON back, again one
response per line. The connection need not be closed between requests, but can be if desired.
By default the connection should be through port 21952, but this can easily be changed, if desired, as
described below.

The example data to be passed to the model, as negotiated between Brodie and Christian, is pretty stable, and the basis for the preliminary implementation.
The example data to be returned from the model seems to be still much in flux, so this preliminary implementation returns what is
essentially a stub value, simply a constant JSON string.

## Hooking the ACT-UP model to this interface

[This section primarily of interest to Christian]

First load into Lisp the source file `model-server.lisp`. Then define a function, `scale:run-model`. Note that `model-server.lisp` does need to be loaded
first so that the symbol `scale:run-model` is available.

The `scale:run-model` function should take three arguments:

* `parameters`, a list structure described further below

* ``raw-data`, a Lisp formatted version of what is provided under this tag by the reasoner; I don't really know what this is all about, so am doing no further formatting on this at present; we may choose to change this later

* `generate-raw-data-p`, a generalized Boolean corresponding to the value provided under the similiar tag by the reasoner; again I don't understand this, but presume being just a Boolean it is unlikely to change.

The value returned by `scale:model-function` will be converted to JSON and written back on the TCP stream to the reasoner.
As the output format appears still to be in flux, for now it is recommended that `scale:model-function` simply return a string; we'll update this when things become clearer.

The `parameters` is an a-list mapping parameter names to p-lists. The parameter names are provided as keywords. The p-lists map attributes of the parameter to values.
Thus, you can get the `:value` of the `:noise` parameter by doing

    (getf (cdr (assoc :noise parameters)) :value)

However, a potentially useful little utility function, `scale:pget` is provided, so you can instead simply do

    (pget parameters :noise)

to get the value. For other attributes you might do something like

    `(pget parameters :noise :parameter-sub-class)

Note that the `:similarity` and `:utility` parameters are handled slightly specially: their `:value`s are p-lists, mapping a keyword version of the type (*e.g.* `:integeer` or `:double`) to a Lisp symbol interned from the string provided in the JSON. By default these symbols are interned in the `CL-USER` package, but if desired this can be changed
by setting the value of the `scale:*data-function-name-package*` variable to a different package name. `CL-USER` was chosen on the assumption that's where
Christian will be working.

Here's an example of the parameters value resulting from first model value provided in `reasoner_to_model_input_spec.json`:

    ((:NOISE :VALUE 0.25 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
      :PARAMETER-SUB-CLASS "architecture")
     (:TEMPERATURE :VALUE 1.0 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
      :PARAMETER-SUB-CLASS "architecture")
     (:SIMILARITY :VALUE
      (:INTEGER COMMON-LISP-USER::FUNCTIONNAME :DOUBLE
       COMMON-LISP-USER::FUNCTIONNAME)
      :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model" :PARAMETER-SUB-CLASS
      "knowledge")
     (:UTILITY :VALUE
      (:INTEGER COMMON-LISP-USER::FUNCTIONNAME :DOUBLE
       COMMON-LISP-USER::FUNCTIONNAME)
      :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model" :PARAMETER-SUB-CLASS "utility")
     (:DECISION :VALUE "functionName" :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
      :PARAMETER-SUB-CLASS "procedure")
     (:INIT-LENGTH :VALUE 10 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "simulation"
      :PARAMETER-SUB-CLASS "simulation")
     (:RUN-LENGTH :VALUE 100 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "simulation"
      :PARAMETER-SUB-CLASS "simulation")
     (:RUN-DELAY :VALUE 1.0 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "simulation"
      :PARAMETER-SUB-CLASS "simulation")
     (:RUN-COUNT :VALUE 100 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "simulation"
      :PARAMETER-SUB-CLASS "simulation")
     (:PROBABILITY-THRESHOLD :VALUE 0.25 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
      "simulation" :PARAMETER-SUB-CLASS "policy")
     (:INTENSITY-THRESHOLD :VALUE 1 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
      "simulation" :PARAMETER-SUB-CLASS "policy")
     (:INTENSITY-STANDARD-DEVIATION :VALUE 1.0 :UNIT-OF-MEASURE NIL
      :PARAMETER-CLASS "simulation" :PARAMETER-SUB-CLASS "environment"))

Note that if no `scale:model-function` has been defined, by default the various values of the input arguments will be printed in the Lisp listener.

Note that this code has only been test in SBCL, though I believe it should run fine in CCL, or any other Common Lisp implementation that supports `usocket`.

## Running this code

[This section is primarily of interest to Brodie]

To run this a suitable Common Lisp implementation (SBCL is recommended) and QuickLisp must be installed.
Installing SBCL is easy on a variety of OSes, so long as a pre-built binary is used.
Lisp and QuickLisp only need to be installed once.

For installing SBCL see [https://www.sbcl.org/getting.html](https://www.sbcl.org/getting.html). It's pretty easy, but
on Linux I (dfm) should be able to help, and probably Macintosh, too, though I've not done it there myself
in many years; for Windows, though, I'm unlikely to be much help, never using it myself.

Once you have a Lisp implementation installed you need to install QuickLisp, which is near trivial.
See [https://www.quicklisp.org/beta/](https://www.quicklisp.org/beta/) for details (don't worry that
it says "beta," it's been around and stable for a very long time). If necessary, I can easily help.

If this is too much I could undoubtedly put together a docker thingie, though that seems like overkill for the purpose.

Once you've got a QuickLisp enabled Lisp installed to run this stuff (on Linux, Macintosh, or any other UNIX-like OS) just do

    ./run.sh

Once it prints a line like

    <INFO> [12:16:43] scale-act-up-interface - Starting SCALE model listener on port 21952

you should be ready to go. Once Christian has supplied the real model this may need a little modification; we'll see.
To make it more loquacious, spitting out lots of debugging status and the like, instead run

    ./debug.sh

To kill the thing just use control-C; I should come down reasonably graciously if you've used SBCL, maybe a little bit less so in other Lisp implementations.

If you want to use a different port just supply the number as the sole argument to `run.sh` or `debug.sh`; for example, to use port 9999

    ./run.sh 9999

## Calling into the interface from the reasoner

[Again, this section is primarily of interest to Brodie]

To use it simply open a TCP connection to relevant port on whatever machine is hosting this software (likely localhost, I presume) and
write and read single lines of JSON to and from it. Note that there can be *no* newlines anywhere in the JSON. If you've got prettily
formatted JSON you'll need to strip the newlines. Note that the JSON spec does not allow newlines within strings, so simply replacing
any in the prettily formatted JSON by spaces works fine. I believe Python's `json.dump()` and `json.dumps()` functions write JSON
with no linefeeds so long as no optional parameters directing it to do otherwise are provided.

If you're new to this world, note that a standard gotcha is that when writing to a TCP stream you typically have to explicitly
flush the IO buffer to send the whole message. There's also potential weirdness in Python in the distinction between strings (which are UTF-8)
and byte vectors, and I think maybe sometimes you have to do an explicit encoding, I no longer remember for sure.
If things do need to be explicitly encoded, they should be UTF-8 (that's a part of the JSON spec, though I doubt we'll have to worry
about anything that's not just vanilla ASCII).

As noted above, I've not done anything with the expected format for the ACT-UP→reasoner stuff, as my understanding is it's still
under negotiation. Right now, for each run of the ACT-UP model the corresponding component of the return value
will be just a meaningless stub string, `"done"`.

As described in in `reasoner_to_model_input_spec.json` the JSON data supplied to the interface will be a JSON object.
Only the `models` slot of this object will be consulted, and it should be a JSON array (in normal-speak we often call these "vectors,"
but the official JSON term is "array"; go figure). The value the interface
returns is a JSON array of the same length. For each element of the input array that is labeled as an `ACT-R` model
it will run the ACT-UP model on it and the corresponding return array element. The elements of the returned array that correspond
to things not marked as being `ACT-R` will be JSON `null` values. Note that with the current stub the non-null values will typically
just be JSON strings, but this will change once we've finished the required negotiation.

Here's an example of testing this stuff using `netcat` to fill in for the reasoner. Note that there is a file, `sample.json`, in this repo.
This contains two JSON values, each on a single line. The first is the example from `reasoner_to_model_input_spec.json`, reduced to a single line;
the second is similar, but with only the first model section of that example data. To test this in one shell start the interface code

    (dfm) dfm@carlisle:~/w/scale/act-up-interface$ ./run.sh
    This is SBCL 2.4.5, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.

    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.
    WARNING: :SB-EVAL is no longer present in *FEATURES*
      <INFO> [12:38:30] scale-act-up-interface - Starting SCALE model listener on port 21952

In a second shell use netcat to send the sample data:

    dfm) dfm@carlisle:~/w/scale/act-up-interface$ cat sample.json | nc 127.0.0.1 21952
    ["done","done"]
    ["done"]

At present, in lieu of the real model, there will also be some output spewed into the first shell
describing what it has seen, mostly as Lisp data, though a little text, too, something like

    parameters: ((:NOISE :VALUE 0.25 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
                  :PARAMETER-SUB-CLASS "architecture")
                 (:TEMPERATURE :VALUE 1.0 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
                  "model" :PARAMETER-SUB-CLASS "architecture")
                 (:SIMILARITY :VALUE
                  (:INTEGER COMMON-LISP-USER::FUNCTIONNAME :DOUBLE
                   COMMON-LISP-USER::FUNCTIONNAME)
                  :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
                  :PARAMETER-SUB-CLASS "knowledge")
                 (:UTILITY :VALUE
                  (:INTEGER COMMON-LISP-USER::FUNCTIONNAME :DOUBLE
                   COMMON-LISP-USER::FUNCTIONNAME)
                  :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
                  :PARAMETER-SUB-CLASS "utility")
                 (:DECISION :VALUE "functionName" :UNIT-OF-MEASURE NIL
                  :PARAMETER-CLASS "model" :PARAMETER-SUB-CLASS "procedure")
                 (:INIT-LENGTH :VALUE 10 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
                  "simulation" :PARAMETER-SUB-CLASS "simulation")
                 (:RUN-LENGTH :VALUE 100 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
                  "simulation" :PARAMETER-SUB-CLASS "simulation")
                 (:RUN-DELAY :VALUE 1.0 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
                  "simulation" :PARAMETER-SUB-CLASS "simulation")
                 (:RUN-COUNT :VALUE 100 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
                  "simulation" :PARAMETER-SUB-CLASS "simulation")
                 (:PROBABILITY-THRESHOLD :VALUE 0.25 :UNIT-OF-MEASURE NIL
                  :PARAMETER-CLASS "simulation" :PARAMETER-SUB-CLASS "policy")
                 (:INTENSITY-THRESHOLD :VALUE 1 :UNIT-OF-MEASURE NIL
                  :PARAMETER-CLASS "simulation" :PARAMETER-SUB-CLASS "policy")
                 (:INTENSITY-STANDARD-DEVIATION :VALUE 1.0 :UNIT-OF-MEASURE NIL
                  :PARAMETER-CLASS "simulation" :PARAMETER-SUB-CLASS "environment"))
    raw-data: "jsonFormattedRawData"
    do not generate raw data

    parameters: ((:NOISE :VALUE 0.25 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
                  :PARAMETER-SUB-CLASS "architecture")
                 (:TEMPERATURE :VALUE 1.0 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
                  "model" :PARAMETER-SUB-CLASS "architecture")
                 (:SIMILARITY :VALUE
                  (:INTEGER COMMON-LISP-USER::FUNCTIONNAME :DOUBLE
                   COMMON-LISP-USER::FUNCTIONNAME)
                  :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
                  :PARAMETER-SUB-CLASS "knowledge"))
    raw-data: "jsonFormattedRawData"
    do not generate raw data

    parameters: ((:NOISE :VALUE 0.25 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
                  :PARAMETER-SUB-CLASS "architecture")
                 (:TEMPERATURE :VALUE 1.0 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
                  "model" :PARAMETER-SUB-CLASS "architecture")
                 (:SIMILARITY :VALUE
                  (:INTEGER COMMON-LISP-USER::FUNCTIONNAME :DOUBLE
                   COMMON-LISP-USER::FUNCTIONNAME)
                  :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
                  :PARAMETER-SUB-CLASS "knowledge")
                 (:UTILITY :VALUE
                  (:INTEGER COMMON-LISP-USER::FUNCTIONNAME :DOUBLE
                   COMMON-LISP-USER::FUNCTIONNAME)
                  :UNIT-OF-MEASURE NIL :PARAMETER-CLASS "model"
                  :PARAMETER-SUB-CLASS "utility")
                 (:DECISION :VALUE "functionName" :UNIT-OF-MEASURE NIL
                  :PARAMETER-CLASS "model" :PARAMETER-SUB-CLASS "procedure")
                 (:INIT-LENGTH :VALUE 10 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
                  "simulation" :PARAMETER-SUB-CLASS "simulation")
                 (:RUN-LENGTH :VALUE 100 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
                  "simulation" :PARAMETER-SUB-CLASS "simulation")
                 (:RUN-DELAY :VALUE 1.0 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
                  "simulation" :PARAMETER-SUB-CLASS "simulation")
                 (:RUN-COUNT :VALUE 100 :UNIT-OF-MEASURE NIL :PARAMETER-CLASS
                  "simulation" :PARAMETER-SUB-CLASS "simulation")
                 (:PROBABILITY-THRESHOLD :VALUE 0.25 :UNIT-OF-MEASURE NIL
                  :PARAMETER-CLASS "simulation" :PARAMETER-SUB-CLASS "policy")
                 (:INTENSITY-THRESHOLD :VALUE 1 :UNIT-OF-MEASURE NIL
                  :PARAMETER-CLASS "simulation" :PARAMETER-SUB-CLASS "policy")
                 (:INTENSITY-STANDARD-DEVIATION :VALUE 1.0 :UNIT-OF-MEASURE NIL
                  :PARAMETER-CLASS "simulation" :PARAMETER-SUB-CLASS "environment"))
    raw-data: "jsonFormattedRawData"
    do not generate raw data

At some point we should think about how to deal with errors. For now, when there is an error in the interface code
a JSON string describing it should be returned.
