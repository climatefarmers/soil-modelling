from pydantic import BaseModel
from fastapi import FastAPI
from typing import List
import os
import csv
from tempfile import TemporaryDirectory
import json
import subprocess

class Params(BaseModel):
    desc: str
    location: str 
    crop: str
    temp_adjustment: float
    soil_thick: int
    clay: float
    c_in: float
    fym_in: float
    pE: float
    time_horizon: int
    evap: List[float]
    precip: List[float]
    temp: List[float]
    bare_profile: List[int]
    SOC_pools: dict

class ModelResults(BaseModel):
    data: dict

app = FastAPI()

@app.post("/rothc/", response_model=ModelResults)
async def rothc_model(params: Params) -> ModelResults:
    with TemporaryDirectory() as temp:
        with open(os.path.join(temp, 'params.json'), 'w') as f:
            json.dump(params.dict(), f)

        subprocess.run([
            "Rscript",
            "--slave",
            "--no-restore",
            "main.R",
            temp,
        ])


        with open(os.path.join(temp, 'out.csv'), 'r') as f:
            data = [r for r in csv.DictReader(f)]
            headers = ["DPM","RPM","BIO","HUM","IOM","TOTAL"]
            v = {k: [round(float(r[k]),3) for r in data] for k in headers}


    return {
        'data' : v
    }