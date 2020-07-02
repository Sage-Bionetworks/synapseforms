FROM rocker/tidyverse:3.6.3

RUN Rscript -e 'install.packages("optparse")'
RUN Rscript -e 'devtools::install_github("Sage-Bionetworks/synapseforms")'
RUN git clone --branch phil-patch-60 https://github.com/philerooski/synapseforms /root/synapseforms
