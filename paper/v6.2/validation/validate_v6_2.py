#!/usr/bin/env python3
"""Independent numerical checks for DAG--Wishart working preprint v6.2.

The script deliberately does not import the historical companion R code.  It
samples directly from the nodewise inverse-gamma/normal representation and
compares Monte Carlo means with the closed-form full precision mean and the
descending full covariance-mean recursion in the corrected preprint.  It also
checks the collider Hausdorff factor, the symmetric-coordinate trace pairing,
and the inverse precision-to-covariance coordinate Jacobian repaired in v6.1.
Version 6.2 changes only the graph artwork, so the mathematical checks remain
the same.
"""

from __future__ import annotations

import argparse
import platform

import numpy as np


def embedded_inverse(U: np.ndarray, indices: list[int]) -> np.ndarray:
    """Embed the inverse of a principal block into a zero full matrix."""
    out = np.zeros_like(U)
    if indices:
        out[np.ix_(indices, indices)] = np.linalg.inv(U[np.ix_(indices, indices)])
    return out


def analytic_precision_mean(
    U: np.ndarray, alpha: np.ndarray, parents: list[list[int]]
) -> np.ndarray:
    p = len(parents)
    out = np.zeros((p, p))
    for i, pa in enumerate(parents):
        family = [i, *pa]
        q = len(pa)
        out += (alpha[i] - q - 2.0) * embedded_inverse(U, family)
        out -= (alpha[i] - q - 3.0) * embedded_inverse(U, pa)
    return out


def node_moments(
    U: np.ndarray, alpha: np.ndarray, parents: list[list[int]]
) -> tuple[list[np.ndarray], np.ndarray, list[np.ndarray]]:
    means: list[np.ndarray] = []
    r = np.empty(len(parents))
    second: list[np.ndarray] = []
    for i, pa in enumerate(parents):
        if not pa:
            means.append(np.empty(0))
            r[i] = U[i, i] / (alpha[i] - 4.0)
            second.append(np.empty((0, 0)))
            continue
        Upa = U[np.ix_(pa, pa)]
        Upa_inv = np.linalg.inv(Upa)
        m = Upa_inv @ U[pa, i]
        cond = U[i, i] - U[i, pa] @ Upa_inv @ U[pa, i]
        r[i] = cond / (alpha[i] - len(pa) - 4.0)
        means.append(m)
        second.append(r[i] * Upa_inv + np.outer(m, m))
    return means, r, second


def analytic_covariance_mean(
    U: np.ndarray, alpha: np.ndarray, parents: list[list[int]]
) -> np.ndarray:
    p = len(parents)
    means, r, second = node_moments(U, alpha, parents)
    out = np.zeros((p, p))
    for i in range(p - 1, -1, -1):
        pa = parents[i]
        if pa:
            for j in range(i + 1, p):
                out[j, i] = out[i, j] = out[j, pa] @ means[i]
            out[i, i] = r[i] + np.trace(out[np.ix_(pa, pa)] @ second[i])
        else:
            out[i, i] = r[i]
    return out


def sample_matrices(
    rng: np.random.Generator,
    n: int,
    U: np.ndarray,
    alpha: np.ndarray,
    parents: list[list[int]],
) -> tuple[np.ndarray, np.ndarray]:
    p = len(parents)
    L = np.broadcast_to(np.eye(p), (n, p, p)).copy()
    D = np.empty((n, p))
    for i, pa in enumerate(parents):
        q = len(pa)
        if pa:
            Upa = U[np.ix_(pa, pa)]
            Upa_inv = np.linalg.inv(Upa)
            m = Upa_inv @ U[pa, i]
            cond = U[i, i] - U[i, pa] @ Upa_inv @ U[pa, i]
        else:
            Upa_inv = np.empty((0, 0))
            m = np.empty(0)
            cond = U[i, i]

        shape = (alpha[i] - q) / 2.0 - 1.0
        scale = cond / 2.0
        # If D ~ IG(shape, scale), then 1/D ~ Gamma(shape, scale=1/scale).
        D[:, i] = 1.0 / rng.gamma(shape, 1.0 / scale, size=n)
        if pa:
            chol = np.linalg.cholesky(Upa_inv)
            beta = m + np.sqrt(D[:, [i]]) * (rng.standard_normal((n, q)) @ chol.T)
            L[:, pa, i] = -beta

    omega = np.einsum("nki,ni,nli->nkl", L, 1.0 / D, L, optimize=True)
    Linv = np.linalg.inv(L)
    sigma = np.einsum("nka,nk,nkb->nab", Linv, D, Linv, optimize=True)
    return omega, sigma


def collider_area_check(rng: np.random.Generator, n: int = 1000) -> float:
    """Check the corrected five-dimensional Hausdorff area factor."""
    worst = 0.0
    for _ in range(n):
        d1, d2, d3 = np.exp(rng.normal(size=3))
        l21, l31 = rng.normal(size=2)
        J = np.array(
            [
                [-d1**-2, 0, 0, 0, 0],
                [-l21 * d1**-2, 0, 0, d1**-1, 0],
                [-l21**2 * d1**-2, -d2**-2, 0, 2 * l21 / d1, 0],
                [-l31 * d1**-2, 0, 0, 0, d1**-1],
                [-l21 * l31 * d1**-2, 0, 0, l31 / d1, l21 / d1],
                [-l31**2 * d1**-2, 0, -d3**-2, 0, 2 * l31 / d1],
            ]
        )
        numeric = np.sqrt(np.linalg.det(J.T @ J))
        closed = (
            d1**-4
            * d2**-2
            * d3**-2
            * np.sqrt((1 + l21**2) * (1 + l31**2))
        )
        worst = max(worst, abs(numeric - closed) / closed)
    return worst


def trace_pairing_check(rng: np.random.Generator) -> float:
    """Verify the half weight required for Euclidean edge coordinates."""
    omega = rng.normal(size=(4, 4))
    omega = (omega + omega.T) / 2.0
    h_diag = rng.normal(size=4)
    edges = [(1, 0), (2, 0), (3, 1), (3, 2)]
    h_edge = rng.normal(size=len(edges))
    K = np.zeros((4, 4))
    np.fill_diagonal(K, h_diag)
    coordinate_pairing = float(h_diag @ np.diag(omega))
    for value, (i, j) in zip(h_edge, edges):
        K[i, j] = K[j, i] = value / 2.0
        coordinate_pairing += value * omega[i, j]
    return abs(np.trace(K @ omega) - coordinate_pairing)


def inverse_coordinate_jacobian_check(
    rng: np.random.Generator, n: int = 100
) -> float:
    """Check |d Sigma^E / d Omega^E| for the collider 2->1<-3.

    The free coordinates are ordered as (11, 21, 22, 31, 33).  The omitted
    moral precision entry is Omega_23=Omega_21 Omega_31/Omega_11.  Complex-step
    differentiation avoids subtraction error and makes this an independent
    numerical check of Lemma ``inverse_mapping``.
    """

    def free_covariance(x: np.ndarray) -> np.ndarray:
        o11, o21, o22, o31, o33 = x
        omega = np.array(
            [
                [o11, o21, o31],
                [o21, o22, o21 * o31 / o11],
                [o31, o21 * o31 / o11, o33],
            ]
        )
        sigma = np.linalg.inv(omega)
        return np.array(
            [sigma[0, 0], sigma[1, 0], sigma[1, 1], sigma[2, 0], sigma[2, 2]]
        )

    worst = 0.0
    h = 1e-30
    eye = np.eye(5)
    for _ in range(n):
        d = np.exp(rng.normal(scale=0.7, size=3))
        l21, l31 = rng.normal(scale=0.8, size=2)
        L = np.array([[1.0, 0.0, 0.0], [l21, 1.0, 0.0], [l31, 0.0, 1.0]])
        omega = L @ np.diag(1.0 / d) @ L.T
        x = np.array(
            [omega[0, 0], omega[1, 0], omega[1, 1], omega[2, 0], omega[2, 2]]
        )

        jac = np.column_stack(
            [
                np.imag(free_covariance(x.astype(complex) + 1j * h * eye[j])) / h
                for j in range(5)
            ]
        )
        numeric = abs(float(np.linalg.det(jac)))
        sigma = np.linalg.inv(omega)
        parents = sigma[np.ix_([1, 2], [1, 2])]
        closed = (
            np.linalg.det(sigma) ** 4
            * sigma[1, 1] ** 2
            * sigma[2, 2] ** 2
            / np.linalg.det(parents) ** 3
        )
        worst = max(worst, abs(numeric - closed) / closed)
    return worst


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--draws", type=int, default=200_000)
    parser.add_argument("--seed", type=int, default=11094371)
    args = parser.parse_args()

    rng = np.random.default_rng(args.seed)
    # Non-perfect four-node DAG: 2->1<-3 and 4->2, 4->3 (one-based labels).
    parents = [[1, 2], [3], [3], []]
    A = np.array(
        [
            [1.40, 0.20, -0.10, 0.30],
            [0.00, 1.20, 0.25, -0.20],
            [0.00, 0.00, 1.10, 0.15],
            [0.00, 0.00, 0.00, 1.30],
        ]
    )
    U = A.T @ A + 0.75 * np.eye(4)
    alpha = np.array([9.0, 8.0, 8.5, 7.5])

    expected_omega = analytic_precision_mean(U, alpha, parents)
    expected_sigma = analytic_covariance_mean(U, alpha, parents)
    omega, sigma = sample_matrices(rng, args.draws, U, alpha, parents)
    observed_omega = omega.mean(axis=0)
    observed_sigma = sigma.mean(axis=0)

    omega_abs = float(np.max(np.abs(observed_omega - expected_omega)))
    sigma_abs = float(np.max(np.abs(observed_sigma - expected_sigma)))
    omega_scaled = omega_abs / float(np.max(np.abs(expected_omega)))
    sigma_scaled = sigma_abs / float(np.max(np.abs(expected_sigma)))
    area_error = collider_area_check(rng)
    pairing_error = trace_pairing_check(rng)
    inverse_jacobian_error = inverse_coordinate_jacobian_check(rng)

    print(f"Python: {platform.python_version()}")
    print(f"NumPy: {np.__version__}")
    print(f"draws: {args.draws}")
    print(f"seed: {args.seed}")
    print(f"E[Omega] max absolute error: {omega_abs:.10g}")
    print(f"E[Omega] max scaled error: {100 * omega_scaled:.6f}%")
    print(f"E[Sigma] max absolute error: {sigma_abs:.10g}")
    print(f"E[Sigma] max scaled error: {100 * sigma_scaled:.6f}%")
    print(f"collider area max relative error: {area_error:.10g}")
    print(f"trace-pairing absolute error: {pairing_error:.10g}")
    print(
        "inverse-coordinate Jacobian max relative error: "
        f"{inverse_jacobian_error:.10g}"
    )

    # Loose stochastic release gates: tight enough to detect sign/scale mistakes,
    # wide enough not to fail because a heavy-tailed Monte Carlo draw is unlucky.
    assert omega_scaled < 0.01
    assert sigma_scaled < 0.02
    assert area_error < 1e-10
    assert pairing_error < 1e-12
    assert inverse_jacobian_error < 1e-10


if __name__ == "__main__":
    main()
