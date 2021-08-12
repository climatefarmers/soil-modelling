import json
import pytest
from fastapi.testclient import TestClient

from main import Params, ModelResults, app

@pytest.fixture()
def test_params():
    with open('./tests/test_params.json', 'r') as f:
        request = Params(**json.load(f))
    with open('./tests/test_output.json', 'r') as f:
        response = json.load(f)
    return (request, response)


def test_rothc(test_params):
    request, response = test_params
    client = TestClient(app)
    res = client.post(
        "/rothc/",
        json=request.dict()
    )
    assert response == res.json()