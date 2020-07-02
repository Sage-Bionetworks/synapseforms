FROM rocker/tidyverse:3.6.3

RUN apt-get update && apt-get install python3-pip -y
RUN pip3 install synapseclient
RUN Rscript -e 'install.packages("optparse")'
RUN Rscript -e 'devtools::install_github("Sage-Bionetworks/synapseforms")'
RUN git clone --branch phil-patch-60 https://github.com/philerooski/synapseforms /root/synapseforms
