FROM rocker/r-base

WORKDIR /code

COPY install_packages.R /code
RUN Rscript install_packages.R

RUN apt-get update && apt-get install -y python3 python3-pip

COPY requirements.txt /code
RUN pip3 install -r requirements.txt

COPY . /code/
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "80"]

