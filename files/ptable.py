import json

with open('ptable.json', 'r') as f:
    ptable = json.loads(f.read())

def get(path):
    d = ptable
    for k in path.split('/'):
        d = d[k]
    return d
