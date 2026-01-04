# Basis map
project_path <- "iPSC_project"

# Maak mappen aan
dirs <- c("data/fastq", "data/bam", "data/counts", "data/qc_reports", "scripts", "results")
for(d in dirs) {
  dir.create(file.path(project_path, d), recursive = TRUE, showWarnings = FALSE)
}

# Sample Run IDs
run_ids <- c("SRR7866687","SRR7866688","SRR7866689","SRR7866690",
             "SRR7866691","SRR7866692","SRR7866693","SRR7866694")

# Maak lege fastq bestanden aan (paired-end)
for(id in run_ids) {
  file.create(file.path(project_path, "data", "fastq", paste0(id, "_1.fastq.gz")))
  file.create(file.path(project_path, "data", "fastq", paste0(id, "_2.fastq.gz")))
}

# Maak lege bam bestanden aan
for(id in run_ids) {
  file.create(file.path(project_path, "data", "bam", paste0(id, ".bam")))
}

# Maak een lege counts file aan
file.create(file.path(project_path, "data", "counts", "counts_table.txt"))

# Maak lege fastqc rapporten aan
for(id in run_ids) {
  file.create(file.path(project_path, "data", "qc_reports", paste0("fastqc_report_", id, ".html")))
}

