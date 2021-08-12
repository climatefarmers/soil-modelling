from pydantic import BaseModel
from fastapi import FastAPI
from typing import List
import os
import csv
from tempfile import TemporaryDirectory
import json
import subprocess

class Params(BaseModel):
    time_horizon: int
    c_in: List[float]
    dr_ratio: List[float]
    bare_profile: List[int]
    soil_thick: int
    clay: float
    pE: float
    evap: List[float]
    precip: List[float]
    temp: List[float]
    SOC_pools: dict
    tilling_factor: float

class ModelResults(BaseModel):
    data: dict

app = FastAPI()

def run_rscript(script, *args):
    subprocess.run([
            "Rscript",
            "--slave",
            "--no-restore",
            script,
            *args
        ])

@app.post("/api/modelling/rothc/", response_model=ModelResults)
async def rothc_model(params: Params) -> ModelResults:
    with TemporaryDirectory() as temp:
        with open(os.path.join(temp, 'params.json'), 'w') as f:
            json.dump(params.dict(), f)


        run_rscript("main.R", temp)        


        with open(os.path.join(temp, 'out.csv'), 'r') as f:
            data = [r for r in csv.DictReader(f)]
            headers = ["DPM","RPM","BIO","HUM","IOM","TOTAL"]
            v = {k: [round(float(r[k]),3) for r in data] for k in headers}

    return {
        'data' : v
    }

@app.get("/")
async def index():
    return {"status": "alive"}

@app.get("/health")
async def healthcheck():
    return {"status": "alive"}

