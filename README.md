# FASTA Processor

FASTA Processor is a browser-based tool for converting FASTA records into a CSV table and checking for duplicate sequence names or duplicate sequence strings.

## Use the App

Open the Shinylive app here:

https://nvelden.github.io/FASTA_processor/

On the first visit, the app can take up to 1-2 minutes to load while the browser downloads and caches the Shinylive runtime. Later visits are usually faster.

## What It Does

- Upload one or more `.fa`, `.fasta`, `.faa`, `.fna`, or `.txt` FASTA files.
- Parse each FASTA record into `source_file`, `name`, `width`, and `sequence` columns.
- Summarize the total number of uploaded files, parsed sequences, duplicate names, and duplicate sequences.
- View duplicate FASTA headers and duplicate sequence strings.
- Download all parsed records or duplicate-only reports as CSV files.

All processing runs in your browser. Uploaded files are not sent to a server.

## Local Development

Run the Shiny app locally:

```r
shiny::runApp(".")
```

Export the static Shinylive site to `docs/`:

```r
stage <- tempfile("shinylive-stage-")
dir.create(stage)
file.copy("app.R", stage)
shinylive::export(stage, "docs")
```

The repository is configured to publish `docs/` to GitHub Pages.
