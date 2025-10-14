FROM rocker/tidyverse:3.6.3-ubuntu18.04

RUN apt-get update && apt-get install python3-pip -y
RUN pip3 install synapseclient==4.8.0
RUN Rscript -e 'install.packages("optparse")'
RUN Rscript -e 'devtools::install_github("philerooski/synapseforms", ref="phil-update-client-auth")'
RUN git clone --branch phil-update-client-auth https://github.com/philerooski/synapseforms /root/synapseforms
