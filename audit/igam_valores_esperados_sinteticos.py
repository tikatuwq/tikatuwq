"""Valores esperados INDEPENDENTES para os testes de integracao do motor IGAM.

Transcricao propria, em Python, das equacoes do SCQA 2005 (SEMAD/UCEMG/PNMA II)
e das regras do Projeto Aguas de Minas, sem usar o codigo R do tikatuwq.
As amostras sao SINTETICAS (nao sao medicoes do IGAM) e foram escolhidas para
percorrer os trechos das curvas.

    python -I audit/igam_valores_esperados_sinteticos.py

Imprime o IQA bruto (17 algarismos), o IQA com 1 casa e a classe IGAM de cada
amostra; esses numeros sao fixados em tests/testthat/test-iqa-igam-api.R.
"""
import math

W = {"od": .17, "mic": .15, "ph": .12, "dbo": .10, "no3": .10, "po4": .10,
     "temp": .10, "tu": .08, "st": .08}
N_TO_NO3 = 62.004 / 14.007
P_TO_PO4 = 94.9714 / 30.9738


def cs(t, cl, h):  # SCQA p. 3
    return (14.2 * math.exp(-0.0212 * t) - 0.0016 * cl * math.exp(-0.0264 * t)) * (0.994 - 0.0001042 * h)


def q_od(p):  # p. 4 (y3 = pi/85 * (p - 15))
    if p <= 100:
        y1 = 0.01396 * p + 0.0873
        y2 = math.pi / 56 * (p - 27)
        y3 = math.pi / 85 * (p - 15)
        y4 = (p - 65) / 10
        y5 = (65 - p) / 10
        return 100 * math.sin(y1) ** 2 - (2.5 * math.sin(y2) - 0.018 * p + 6.86) * math.sin(y3) + 12 / (math.exp(y4) + math.exp(y5))
    if p <= 140:
        return -0.00777142857142832 * p ** 2 + 1.27854285714278 * p + 49.8817148572
    return 47.0


def q_mic(c):  # p. 5
    if c > 1e5:
        return 3.0
    L = math.log10(c)
    return 98.24034 - 34.7145 * L + 2.614267 * L ** 2 + 0.107821 * L ** 3


def q_ph(x):  # p. 6
    if x <= 2:
        return 2.0
    if x <= 6.9:
        return -37.1085 + 41.91277 * x - 15.7043 * x ** 2 + 2.417486 * x ** 3 - 0.091252 * x ** 4
    if x <= 7.1:
        return -4.69365 - 21.4593 * x - 68.4561 * x ** 2 + 21.638886 * x ** 3 - 1.59165 * x ** 4
    if x <= 12:
        return -7698.19 + 3262.031 * x - 499.494 * x ** 2 + 33.1551 * x ** 3 - 0.810613 * x ** 4
    return 3.0


def q_dbo(x):  # p. 7
    return 100.9571 - 10.7121 * x + 0.49544 * x ** 2 - 0.011167 * x ** 3 + 0.0001 * x ** 4 if x <= 30 else 2.0


def q_no3(x):  # p. 8 (mg/L NO3)
    if x <= 10:
        return -5.1 * x + 100.17
    if x <= 60:
        return -22.853 * math.log(x) + 101.18
    if x <= 90:
        return 1e10 * x ** -5.1161
    return 1.0


def q_po4(x):  # p. 9
    return 79.7 * (x + 0.821) ** -1.15 if x <= 10 else 5.0


def q_tu(t):  # p. 10, coeficiente 1,5 (radianos)
    if t <= 100:
        return 90.37 * math.exp(-0.0169 * t) - 1.5 * math.cos(0.0571 * (t - 30)) + 10.22 * math.exp(-0.231 * t) - 0.8
    return 5.0


def q_st(s):  # p. 11
    if s <= 500:
        return 133.17 * math.exp(-0.0027 * s) - 53.17 * math.exp(-0.0141 * s) - 6.2 * math.exp(-0.00462 * s) * math.sin(0.0146 * s)
    return 30.0


def classe(v):  # SCQA Tabela 2.2
    if v <= 25:
        return "Muito Ruim"
    if v <= 50:
        return "Ruim"
    if v <= 70:
        return "Medio"
    if v <= 90:
        return "Bom"
    return "Excelente"


def iqa(s, nitrate_unit, p_basis):
    q = {}
    q["od"] = q_od(100 * s["od"] / cs(s["t"], s["cl"], s["h"]))
    q["mic"] = q_mic(s["mic"])
    q["ph"] = q_ph(s["ph"])
    q["dbo"] = q_dbo(s["dbo"])
    q["no3"] = q_no3(s["no3"] * (N_TO_NO3 if nitrate_unit == "N" else 1.0))
    q["po4"] = q_po4(s["p"] * (P_TO_PO4 if p_basis == "P" else 1.0))
    q["temp"] = 92.0
    q["tu"] = q_tu(s["tu"])
    if s["st"] is not None:
        q["st"] = q_st(s["st"])
    # Regra IGAM: peso dos ausentes repartido igualmente entre os presentes
    miss = sum(w for k, w in W.items() if k not in q)
    w = {k: W[k] + miss / len(q) for k in q}
    v = 1.0
    for k in q:
        v *= q[k] ** w[k]
    return v


# Conjunto A: nitrato em mg/L N, fosforo total em mg/L P, E. coli
A = {
    "A1": dict(od=7.0, t=25, cl=10, h=0, mic=100, ph=7.3, dbo=2, no3=1.0, p=0.10, tu=5, st=100),
    "A2": dict(od=3.0, t=20, cl=50, h=800, mic=200000, ph=6.5, dbo=35, no3=5.0, p=0.5, tu=150, st=600),
    "A3": dict(od=12.0, t=15, cl=5, h=300, mic=10, ph=8.5, dbo=10, no3=15.0, p=4.0, tu=50, st=300),
    "A4": dict(od=7.0, t=25, cl=10, h=0, mic=100, ph=7.3, dbo=2, no3=1.0, p=0.10, tu=5, st=None),
    "A5": dict(od=20.0, t=10, cl=0, h=0, mic=1, ph=2.0, dbo=0, no3=0.02, p=0.01, tu=0, st=0),
}
# Conjunto B: nitrato em mg/L NO3, fosfato total em mg/L PO4, termotolerantes
B = {
    "B1": dict(od=6.0, t=22, cl=20, h=500, mic=1000, ph=7.0, dbo=5, no3=8.0, p=0.3, tu=30, st=250),
    "B2": dict(od=9.5, t=18, cl=2, h=1000, mic=50, ph=9.5, dbo=1, no3=95.0, p=11.0, tu=100, st=500),
}
for name, s in list(A.items()) + list(B.items()):
    v = iqa(s, "N" if name in A else "NO3", "P" if name in A else "PO4")
    r = round(v, 1)
    gap = abs((v * 10) % 1 - 0.5) / 10
    print(f"{name}: bruto={v:.17g}  arredondado={r:.1f}  classe={classe(r)}  dist_desempate={gap:.4f}")
