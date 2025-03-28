# SCALE project interface between the reasoner and the ACT-UP model

This is a preliminary pass at an interface between the reasoner and the ACT-UP model for the SCALE project.
It assumes the reasoner will pass data to the ACT-UP model, and synchronously read a return value from it.
In particular, the reasoner should open a TCP connection to the model, and send line delimited JSON through
this TCP connection as UTF-8 text, one request per line, and read line delimited JSON back, again one
response per line. The connection need not be closed between requests, but can be if desired.
By default the connection should be through port 21952, but this can easily be changed, if desired, as
described below.

The example data to be passed, both from the reasoner to the model, and back to the reasoner from the model,
as negotiated between Brodie and Christian, is pretty stable, and the basis for this implementation.

## Hooking the ACT-UP model to this interface

[This section primarily of interest to Christian]

First load into Lisp the source file `model-server.lisp`. Then define a function, `scale:run-model`. Note that `model-server.lisp` does need to be loaded
first so that the symbol `scale:run-model` is available.

The `scale:run-model` function should take two arguments:

* `parameters`, a list structure described further below

* ``raw-data`, a Lisp formatted version of what is provided under this tag by the reasoner; I don't really know what this is all about, so am doing no further formatting on this at present; we may choose to change this later

The first value returned by `scale:run-model` will be converted to JSON and written back on the TCP stream to the reasoner.

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

Note that if no `scale:run-model` has been defined, by default the various values of the input arguments will simply be printed in the Lisp listener.

The first return value should be something isomorphic to a sequence (*i.e.* a list or vector) of sequences, the elements of the inner sequence being lists of the form for the input parameters, described above.
Note that for these purposes a two dimensional array is considered isomorphic to a sequence of sequences.
The resulting JSON is assembled into a structure looking like that in `model_to_reasoner_output_spec.json`.
Since only the `ACT-R` models in the input are run through the ACT-UP model, all non-ACT-R models in the return value are replaced by JSON `null` values.

The second value returned by the `model-function` should be the `behavior` name to be included in the returned JSON; if it is `nil`, or the `run-mdoel` function only
returns a single value, the constant `evacuate/stay` is used, as in the first such value in Brody's example, thought this constant can easily be
changed if desired.

Note that if no real `run-mdoel` function is provided on the Lisp side a default value, as provided by Christian, is used. This default
value is stored in the repo as `output-sample.lisp`. If the contents of this file are changed the effect will be immediate in the code,
no need to restart anything, as it reads the file each time it needs this value.

Note that this code has only been tested in SBCL, though I believe it should run fine in CCL, or any other Common Lisp implementation that supports `usocket`.

## Calling into the interface from the reasoner

[This section is primarily of interest to Brodie]

To use it simply open a TCP connection to relevant port on whatever machine is hosting this software.
Currently it is hosted on `koalemos.lan.cmu.edu`, using the default port of 21952.
So you could open such a connection
(on Linux or another UNIX-like OS, I don't know what the corresponding incantation would be in Windows) by simply doing

    nc koalemos.lan.cmu.edu 21952

Alternatively you can run the code locally (as described in the next section), and connect to it through localhost.

Then write and read single lines of JSON to and from it. Note that there can be *no* newlines anywhere in the JSON. If you've got prettily
formatted JSON you'll need to strip the newlines. Note that the JSON spec does not allow newlines within strings, so simply replacing
any in the prettily formatted JSON by spaces works fine. I believe Python's `json.dump()` and `json.dumps()` functions write JSON
with no linefeeds so long as no optional parameters directing it to do otherwise are provided.

If you're new to this world, note that a standard gotcha is that when writing to a TCP stream from code (C, Python, whatever) you typically have to explicitly
flush the IO buffer to send the whole message. There's also potential weirdness in Python in the distinction between strings (which are UTF-8)
and byte vectors, and I think maybe sometimes you have to do an explicit encoding, I no longer remember for sure.
If things do need to be explicitly encoded, they should be UTF-8 (that's a part of the JSON spec, though I doubt we'll have to worry
about anything that's not just vanilla ASCII).

As described in in `reasoner_to_model_input_spec.json` the JSON data supplied to the interface will be a JSON object.
Only the `models` slot of this object will be consulted, and it should be a JSON array (in normal-speak we often call these "vectors,"
but the official JSON term is "array"; go figure). The value the interface
returns is as described in `model_to_reasoner_output_spec.json`, with one `Models` entry per corresponding entry in the input, albeit
with all non-ACT-R models have a value of JSON `null`.

## Running this code

[Again, this section is primarily of interest to Brodie]

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
Once Christian has supplied the real model this may need a little modification; we'll see.
To make it more loquacious, spitting out lots of debugging status and the like, instead run

    ./debug.sh

In Windows these shell scripts won't work, and you'll have to make a suitable replacement. I doubt it would be difficult, but I'm clueless about all things Windows.

To kill the thing just use control-C; it should come down reasonably gracefully if you've used SBCL, perhaps with a little less grace in other Lisp implementations.

If you want to use a different port just supply the number as the sole argument to `run.sh` or `debug.sh`; for example, to use port 9999

    ./run.sh 9999

## Examples

Here's an example of testing this stuff using `netcat` to fill in for the reasoner. Note that there is a file, `input-sample.json`, in this repo.
This contains two JSON values, each on a single line. The first is the example from `reasoner_to_model_input_spec.json`, reduced to a single line;
the second is similar, but with only the first model section of that example data. We send this to the TCP server running on Koalemos:

    (dfm) dfm@carlisle:~/w/scale/act-up-interface$ cat input-sample.json | nc koalemos.lan.cmu.edu 21952

This prints two lines of JSON, as follows. Since these are all on one line they are difficult to read. Following this I will copy one of them more tidily formatted. Note also that these are currently constant return values since the real ACT-UP model is not yet hooked up; this is just for testing the interface:

    {"models":[{"name":"ACT-R","behavior":[{"name":"evacuate/stay","runs":[[[{"name":"decision","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.32606483,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":2,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-5,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.8650311,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":3,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-3,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.005523855,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":1,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":5,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.5493901,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":4,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":5,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":0,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.28204924,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":4,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":1,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":4,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}]],[[{"name":"decision","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.46812302,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":1,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-5,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.0062158704,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":3,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":2,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":3,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.9267006,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":1,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":2,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-2,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.4891229,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":2,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":3,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-2,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.7557405,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":0,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}]]]}]},{"name":"ACT-R","behavior":[{"name":"evacuate/stay","runs":[[[{"name":"decision","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.32606483,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":2,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-5,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.8650311,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":3,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-3,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.005523855,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":1,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":5,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.5493901,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":4,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":5,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":0,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.28204924,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":4,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":1,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":4,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}]],[[{"name":"decision","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.46812302,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":1,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-5,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.0062158704,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":3,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":2,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":3,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.9267006,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":1,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":2,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-2,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.4891229,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":2,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":3,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-2,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.7557405,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":0,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}]]]}]}]}
    {"models":[{"name":"ACT-R","behavior":[{"name":"evacuate/stay","runs":[[[{"name":"decision","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.32606483,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":2,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-5,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.8650311,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":3,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-3,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.005523855,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":1,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":5,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.5493901,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":4,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":5,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":0,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.28204924,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":4,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":1,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":4,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}]],[[{"name":"decision","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.46812302,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":1,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-5,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.0062158704,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":3,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":2,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":3,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.9267006,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":1,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":2,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-2,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.4891229,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":2,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":3,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":-2,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}],[{"name":"decision","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"action"},{"name":"evacuation_message","value":0.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"probability","value":0.7557405,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"intensity","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"context"},{"name":"landing","value":1.0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"damage","value":0,"unitOfMeasure":"double","outputClass":"observable","outputSubClass":"outcome"},{"name":"utility","value":0,"unitOfMeasure":"double","outputClass":"internal","outputSubClass":"utility"}]]]}]}]}

If you instead ran the Lisp TCP server locally, there will also be some output spewed into the first shell
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

As promised above, here's the first value returned above, but reformatted prettily on multiple lines:

    {
      "models": [
        {
          "name": "ACT-R",
          "behavior": [
            {
              "name": "evacuate/stay",
              "runs": [
                [
                  [
                    {
                      "name": "decision",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.32606483,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 2,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": -5,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.8650311,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 3,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": -3,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.005523855,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": 5,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.5493901,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 4,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 5,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.28204924,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 4,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": 4,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ]
                ],
                [
                  [
                    {
                      "name": "decision",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.46812302,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": -5,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.0062158704,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 3,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 2,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": 3,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.9267006,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 2,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": -2,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.4891229,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 2,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 3,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": -2,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.7557405,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ]
                ]
              ]
            }
          ]
        },
        {
          "name": "ACT-R",
          "behavior": [
            {
              "name": "evacuate/stay",
              "runs": [
                [
                  [
                    {
                      "name": "decision",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.32606483,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 2,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": -5,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.8650311,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 3,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": -3,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.005523855,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": 5,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.5493901,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 4,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 5,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.28204924,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 4,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": 4,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ]
                ],
                [
                  [
                    {
                      "name": "decision",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.46812302,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": -5,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.0062158704,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 3,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 2,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": 3,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.9267006,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 2,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": -2,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.4891229,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 2,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 3,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": -2,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ],
                  [
                    {
                      "name": "decision",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "action"
                    },
                    {
                      "name": "evacuation_message",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "probability",
                      "value": 0.7557405,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "intensity",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "context"
                    },
                    {
                      "name": "landing",
                      "value": 1,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "damage",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "observable",
                      "outputSubClass": "outcome"
                    },
                    {
                      "name": "utility",
                      "value": 0,
                      "unitOfMeasure": "double",
                      "outputClass": "internal",
                      "outputSubClass": "utility"
                    }
                  ]
                ]
              ]
            }
          ]
        }
      ]
    }



## Error handling

At some point we should think about how to deal with errors. For now, when there is an error in the interface code
it will simply attempt to return a JSON string describing it. For example,

    (dfm) dfm@carlisle:~/w/scale/act-up-interface$ echo '{"ill-formated": "JSON"]' | nc koalemos.lan.cmu.edu 21952

prints

    Error: Expected a `,' separator or `}' in Object on JSON input but found `]' [in #<dynamic-extent STRING-INPUT-STREAM (unavailable) from "{\"ill-fo..."> at position 24]
