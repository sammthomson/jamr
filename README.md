JAMR - AMR Parser
=================

JAMR is a semantic parser for the [Abstract Meaning Representation](http://amr.isi.edu/).

- We will be providing trained models in a ready-to-go package soon.


Building
========

JAMR depends on

 * the [Illinois NER system](http://cogcomp.cs.illinois.edu/page/download_view/NETagger) v2.3,
 * the tokenization scripts in [cdec](https://github.com/redpony/cdec), and 
 * the [Stanford Parser](http://nlp.stanford.edu/software/lex-parser.shtml).

Install these dependencies, and then change the environment variables in
`scripts/config.sh` to point at their executables.
Source `. ./scripts/config.sh`, so that other bash scripts have access to these envvars (JAMR has only been tested with the `bash` shell).
 
Run `./compile` to build an uberjar, which will be output to
    `target/scala-{scala_version}/jamr-assembly-{jamr_version}.jar`


Preprocessing
=============

Download `LDC2013E117.tgz` from the LDC Catalog (requires an LDC subscription).
Extract the file `deft-amr-release-r3-proxy.txt` into `data/LDC-2013-Sep/`.

    cd scripts/preprocessing

Run `./PREPROCESS.sh` (not tested).


Training
========

    cd scripts/training

Extract concept table:

    ./cmd.conceptTable.train

Concept identification (stage1) training:

    ./cmd.stage1-weights

Relation identification (stage2) training:

    ./cmd.stage2-weights

(Search for 'Performance on Dev' in `stage2-weights.err` for early stopping)


Evaluating
==========

Decode test set:

    ./cmd.test.decode.allstages

  or

    ./cmd.test.decode.stage2only

Evaluate the predictions using smatch:

    ./scripts/smatch_v1_0/smatch_modified.py --pr -f test.decode.allstages data/LDC-2013-Sep/amr-release-proxy.test
