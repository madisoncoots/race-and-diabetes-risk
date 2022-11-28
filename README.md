# race-and-diabetes-risk
Replication code for Aggarwal et al. and extension code

Code for the replication of Aggarwal et al. (model creation and the creation of subsequent figures and downstream results) is located in `replication_code/`.
Extension code that builds on the analysis of Aggarwal et al. is located in `extension_code/`.

To easily download the data files necessary for the replication AND extension, download and run `make_replication_data.R` and `pull_extension_data.R`. Be sure to update the `output_path_string` variable at the end of the file so that the files save to the intended location in your directory. Additionally, be sure to update the `data_path` to the same path in each of the analytic files in `replication_code/` and `extension_code/` so that these scripts read the data from the intended directory.

Questions or comments about this code repository can be sent to Madison at mcoots@g.harvard.edu.
