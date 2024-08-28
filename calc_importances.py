import os
import sys

import pandas as pd

from fanova import fANOVA

x_file = sys.argv[1]
y_file = sys.argv[2]
out_file = sys.argv[3]

print(x_file, y_file, out_file)

x = pd.read_csv(x_file)
y = pd.read_csv(y_file)


if "model_id" in x.columns:
    x = x.join(pd.get_dummies(x.model_id, prefix="model")).drop("model_id", axis=1)

if "dataset" in x.columns:
    x = x.join(pd.get_dummies(x.dataset, prefix="dataset")).drop("dataset", axis=1)

for c in x:
    x[c] = x[c].astype(float)

f = fANOVA(x.to_numpy(), y.to_numpy())

a = f.quantify_importance((0, 1, 2, 3, 4))
print(a)

df = pd.DataFrame(a).transpose()

B, E, LR, C, q = [], [], [], [], []

for i in df.index:
    B.append("(0," in str(i))
    E.append("1" in str(i))
    LR.append("2" in str(i))
    C.append("3" in str(i))
    q.append("4" in str(i))

df.insert(0, "q", q)
df.insert(0, "C", C)
df.insert(0, "lr", LR)
df.insert(0, "epochs", E)
df.insert(0, "bs", B)

df = df.reset_index(drop=True)

df.to_csv(out_file, index=False)
