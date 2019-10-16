Code is ready and is added.
Code should be run in R.
The R-data file "Goldstein.Rdata" contains the data in R-format
The file "General.R" contains some general functions that are used in more than one script.
The file "main_autoencoder.R" contains the code to apply the autoencoder algorithm on the benchmark.
The file "main_iforest.R" contains the code to apply the isolation forest algorithm on the benchmark.
The file "main_rbm.R" contains the code to apply the rbm algorithm on the benchmark. The actual rbm was adjusted slightly and can be found in my_rbm.R
After running all these scripts, run "analyze_results.R". This should give the reported auc's of the paper.

Note: some minor parts of the scripts have been commented out such that the code will not run exceptional long when run for the first time.  

