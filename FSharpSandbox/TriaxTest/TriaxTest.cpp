// TriaxTest.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <vector>
#include <iostream>
#include <assert.h>

using namespace std;

static const int MDIM = 3;
static const double PI = 3.141592654;

void make_kron_delta(double kron_delta[MDIM][MDIM]) {
  for (int i = 0; i<MDIM; i++) {
    for (int j = 0; j<MDIM; j++) {
      if (i == j) kron_delta[i][j] = 1;
      else kron_delta[i][j] = 0;
    }
  }
}

double scalar_dabs(double k) {
  double tmp;
  if (k >= 0) tmp = k;
  if (k<0) tmp = -1 * k;
  return tmp;
}

double giverad(double deg) {
  return deg*PI / 180;
}

double givedeg(double rad) {
  return rad * 180 / PI;
}

int max(vector<int> vektor) {
  int max = -32000;
  for (int i = 0; i<vektor.size(); i++) {
    if (vektor[i]>max) max = vektor[i];
  }
  return max;
}

int min(vector<int> vektor) {
  int min = 32000;
  for (int i = 0; i<vektor.size(); i++) {
    if (vektor[i]<min) min = vektor[i];
  }
  return min;
}

/*
nasledujici funkce byly prevzaty z programu Tochnog: http://tochnog.sourceforge.net
Copyright (C) 1998  Dennis Roddeman
email: dennis.roddeman@feat.nl
*/

const double EPS_Q = 1.e-10;

void array_add(double a[], double b[], double c[], long int n) {
  register long int i = 0;
  for (i = 0; i<n; i++) c[i] = a[i] + b[i];
}

double array_inproduct(double a[], double b[], long int n) {
  register long int i = 0;
  double result = 0.;
  for (i = 0; i<n; i++) result += a[i] * b[i];
  return result;
}

void array_move(double from[], double to[], long int n) {
  register long int i = 0;
  for (i = 0; i<n; i++) to[i] = from[i];
}

void array_multiply(double a[], double b[], double c, long int n) {
  register long int i = 0;
  for (i = 0; i<n; i++) b[i] = c * a[i];
}

void array_set(double *ptr, double value, long int n) {
  register long int i = 0;
  for (i = 0; i<n; i++) *(ptr + i) = value;
}

double array_size(double a[], long int n) {
  double size = 0.;
  size = sqrt(scalar_dabs(array_inproduct(a, a, n)));
  return size;
}

void array_subtract(double a[], double b[], double c[], long int n) {
  register long int i = 0;
  for (i = 0; i<n; i++) c[i] = a[i] - b[i];
}

void matrix4_ab(double a[], double b[], double c[3][3][3][3]) {
  register long int i = 0, j = 0, k = 0, l = 0;

  for (i = 0; i<3; i++) {
    for (j = 0; j<3; j++) {
      for (k = 0; k<3; k++) {
        for (l = 0; l<3; l++) {
          c[i][j][k][l] = a[i * 3 + j] * b[k * 3 + l];
        }
      }
    }
  }

}

void matrix_a4b(double a[3][3][3][3], double b[], double c[])

{
  register long int i = 0, j = 0, k = 0, l = 0;

  for (i = 0; i<3; i++) {
    for (j = 0; j<3; j++) {
      c[i * 3 + j] = 0.;
      for (k = 0; k<3; k++) {
        for (l = 0; l<3; l++) {
          c[i * 3 + j] += a[i][j][k][l] * b[k * 3 + l];
        }
      }
    }
  }

}

double trace(double a[3][3]) {
  return(a[0][0] + a[1][1] + a[2][2]);
}

void matrix_a_contr_b(double a[], double b[], double &c) {
  register long int i = 0, j = 0;
  c = 0;

  for (i = 0; i<3; i++) {
    for (j = 0; j<3; j++) {
      c += a[i * 3 + j] * b[i * 3 + j];
    }
  }

}

void matrix_ab4(double a[], double b[3][3][3][3], double c[]) {
  register long int i = 0, j = 0, k = 0, l = 0;

  for (i = 0; i<3; i++) {
    for (j = 0; j<3; j++) {
      c[i * 3 + j] = 0.;
      for (k = 0; k<3; k++) {
        for (l = 0; l<3; l++) {
          c[i * 3 + j] += a[k * 3 + l] * b[k][l][i][j];
        }
      }
    }
  }

}

void matrix_ab(double *a, double *b, double *c, long int n, long int m,
  long int k)
  // c[n][k] = a[n][m] * b[m][k]
{
  register long int i = 0, j = 0, l = 0;

  for (i = 0; i<n; i++) {
    for (j = 0; j<k; j++) {
      *(c + i*k + j) = 0;
      for (l = 0; l<m; l++) {
        *(c + i*k + j) += (*(a + i*m + l)) *
          (*(b + l*k + j));
      }
    }
  }
}


double matrix_determinant(double a[], long int n) {

  double result = 0.;

  if (n == 1)
    result = a[0];
  else if (n == 2)
    result = a[0] * a[3] - a[1] * a[2];
  else {
    assert(n == 3);
    result = a[0] * (a[4] * a[8] - a[7] * a[5]) -
      a[1] * (a[3] * a[8] - a[6] * a[5]) + a[2] * (a[3] * a[7] - a[6] * a[4]);
  }
  return result;
}

void matrix_invariants(double *mat, double *inv) {

  inv[0] = mat[0] + mat[4] + mat[8];
  inv[1] = mat[0] * mat[4] + mat[4] * mat[8] + mat[8] * mat[0] -
    mat[1] * mat[3] - mat[5] * mat[7] - mat[6] * mat[2];
  inv[2] = matrix_determinant(mat, 3);

}

void matrix_eigenvalues(double mat[], double eigenvalues[]) {

  double I1 = 0., I2 = 0., I3 = 0., r = 0., s = 0., t = 0., p = 0., q = 0.,
    bigR = 0., phi = 0., y0 = 0., y1 = 0., y2 = 0., tmp = 0., inv[3];

  matrix_invariants(mat, inv);
  I1 = inv[0];
  I2 = inv[1];
  I3 = inv[2];
  r = -I1;
  s = +I2;
  t = -I3;
  p = (3.*s - r*r) / 3.;
  q = 2.*r*r*r / 27. - r*s / 3. + t;
  if (scalar_dabs(q)<EPS_Q) {
    y0 = -sqrt(scalar_dabs(p));
    y1 = +sqrt(scalar_dabs(p));
    y2 = 0.;
  }
  else {
    bigR = sqrt(scalar_dabs(p) / 3.); if (q<0.) bigR = -bigR;
    tmp = q / (2.*bigR*bigR*bigR);
    if (tmp<-1.) tmp = -1.;
    if (tmp>+1.) tmp = +1.;
    phi = acos(tmp);
    y0 = -2.*bigR*cos(phi / 3.);
    y1 = -2.*bigR*cos(phi / 3. + 2.*PI / 3.);
    y2 = -2.*bigR*cos(phi / 3. + 4.*PI / 3.);
  }
  eigenvalues[0] = y0 - r / 3.;
  eigenvalues[1] = y1 - r / 3.;
  eigenvalues[2] = y2 - r / 3.;
}

double scalar_power(double a, double b) {
  if (b<0.500001 && b>0.499999 && a<0) {
    cout << " Nelze pocitat druhou odmocninu ze zaporneho cisla";
    exit(0);
  }
  double result = 0.;
  if (b == 0.)
    result = 1.;
  else
    result = pow(a, b);
  return result;
}

double scalar_square(double a) {
  return a * a;
}
bool string_isdouble(string word) {
  bool result = true;

  for (int i = 0; i<word.size(); i++) {
    if (!isdigit(word[i])) {
      if (word[i] != '-' && word[i] != '+' &&
        word[i] != 'e' && word[i] != '.' && word[i] != 'E') result = false;
    }
  }

  return result;
}

//// DAVIDUV MODEL ////
void give_LN(double phic, double lambda, double kappa, double Nparam, double nuparam) {

  double hypo_L[MDIM][MDIM][MDIM][MDIM];
  double hypo_N[MDIM][MDIM];

  double kron_delta[MDIM][MDIM];
  for (int i = 0; i < MDIM; i++)
    for (int j = 0; j < MDIM; j++)
      if (i == j)
        kron_delta[i][j] = 1.0;
      else
        kron_delta[i][j] = 0.0;



  // Stress
  double T[MDIM][MDIM];
  for (int i = 0; i < MDIM; i++)
    for (int j = 0; j < MDIM; j++)
    {
      T[i][j] = 0.0;
      if (i == j) T[i][j] = -100.0;
      if (i == 0 && j == 0) T[i][j] = -200.0;
    }
  
  // State
  double e = 0.5;

  //array_set(&T[0][0], 0, MDIM*MDIM);
  //array_move(vari.sig, &T[0][0], MDIM*MDIM);

  // Defines anisotropy
  double alphanu = 1.0;
  double alphaE = 1.0;
  double alphaG = 1.0;

  double pt = 0; // Isotropic offsett
  
  T[0][0] -= pt;
  T[1][1] -= pt;
  T[2][2] -= pt;

  double peast = exp((Nparam - log(1 + e)) / lambda);
  double T_star[MDIM][MDIM];
  double p = -trace(T) / 3;
  array_move(&T[0][0], &T_star[0][0], MDIM*MDIM);
  for (int idim = 0; idim<MDIM; idim++) T_star[idim][idim] += p;
  double T_str_star[MDIM][MDIM];
  array_multiply(&T_star[0][0], &T_str_star[0][0], (1 / trace(T)), MDIM*MDIM);
  double T_str[MDIM][MDIM];
  array_multiply(&T[0][0], &T_str[0][0], (1 / trace(T)), MDIM*MDIM);
  double I1 = trace(T);
  double I2 = (array_inproduct(&T[0][0], &T[0][0], MDIM*MDIM) - I1*I1) / 2;
  double I3 = matrix_determinant(&T[0][0], 3);

  double fd = (2 * p) / peast;
  double cos2phic = 1 - sin(phic)*sin(phic);
  double sin2phim = (9 * I3 + I1*I2) / (I3 + I1*I2);
  double ashape = 0.3;
  double npow = -log(cos2phic) / log(2) + ashape*(sin2phim - sin(phic)*sin(phic));
  double fdsbs = 2 * scalar_power((-8 * I3 / (I3 + I1*I2)), (1 / npow));

  double a = sqrt(3.)*(3 - sin(phic)) / (2 * sqrt(2.)*sin(phic));
  // This differs from the paper
  double alpha_power = log((lambda - kappa)*(3.0 + a*a) / ((lambda + kappa)*a*sqrt(3.0))) / log(2.0);
  //double alpha_power = 2.0;

  fd = scalar_power(fd, alpha_power);
  fdsbs = scalar_power(fdsbs, alpha_power);

  double fddivfdA = fd / fdsbs;

  bool isotrop = false;
  if (sin2phim<1.e-10) {
    sin2phim = 0;
    isotrop = true;
  }
  double nuhh = nuparam;
  double nuvh = nuhh / alphanu;

  double Am = nuvh*nuvh*(4 * alphaE*alphanu - 2 * alphaE*alphaE*alphanu*alphanu + 2 * alphaE*alphaE - alphanu*alphanu) + nuvh*(4 * alphaE + 2 * alphaE*alphanu) + 1 + 2 * alphaE;
  double fs = 9 * p / 2 * (1 / kappa + 1 / lambda) / Am;

  double a1 = alphaE*(1 - alphanu*nuvh - 2 * alphaE*nuvh*nuvh);
  double a2 = alphaE*nuvh*(alphanu + alphaE*nuvh);
  double a3 = alphaE*nuvh*(1 + alphanu*nuvh - alphanu - alphaE*nuvh);
  double a4 = (1 - alphanu*nuvh - 2 * alphaE*nuvh*nuvh)*(alphaE*(1 - alphaG)) / alphaG;
  double a5 = alphaE*(1 - alphaE*nuvh*nuvh) + 1 - alphanu*alphanu*nuvh*nuvh - 2 * alphaE*nuvh*(1 + alphanu*nuvh) - 2 * alphaE*(1 - alphanu*nuvh - 2 * alphaE*nuvh*nuvh) / alphaG;

  array_set(&hypo_L[0][0][0][0], 0, MDIM*MDIM*MDIM*MDIM);

  double nvect[3];
  array_set(nvect, 0, 3);
  nvect[0] = 1;//vertical direction is "0"
  double pmat[3][3];
  array_set(&pmat[0][0], 0, 9);
  for (int i = 0; i<MDIM; i++) {
    for (int j = 0; j<MDIM; j++) {
      pmat[i][j] = nvect[i] * nvect[j];
    }
  }
  double kck[3][3][3][3];
  double kdk[3][3][3][3];
  double pdk[3][3][3][3];
  double kdp[3][3][3][3];
  double pck[3][3][3][3];
  double pdp[3][3][3][3];
  array_set(&kck[0][0][0][0], 0, 81);
  array_set(&kdk[0][0][0][0], 0, 81);
  array_set(&pdk[0][0][0][0], 0, 81);
  array_set(&kdp[0][0][0][0], 0, 81);
  array_set(&pck[0][0][0][0], 0, 81);
  array_set(&pdp[0][0][0][0], 0, 81);
  for (int i = 0; i<3; i++) {
    for (int j = 0; j<3; j++) {
      for (int k = 0; k<3; k++) {
        for (int l = 0; l<3; l++) {
          kck[i][j][k][l] = (kron_delta[i][k] * kron_delta[j][l] + kron_delta[i][l] * kron_delta[j][k] + kron_delta[j][l] * kron_delta[i][k] + kron_delta[j][k] * kron_delta[i][l]) / 2;
          kdk[i][j][k][l] = kron_delta[i][j] * kron_delta[k][l];
          pdk[i][j][k][l] = pmat[i][j] * kron_delta[k][l];
          kdp[i][j][k][l] = kron_delta[i][j] * pmat[k][l];
          pck[i][j][k][l] = (pmat[i][k] * kron_delta[j][l] + pmat[i][l] * kron_delta[j][k] + pmat[j][l] * kron_delta[i][k] + pmat[j][k] * kron_delta[i][l]) / 2;
          pdp[i][j][k][l] = pmat[i][j] * pmat[k][l];
        }
      }
    }
  }
  for (int i = 0; i<3; i++) {
    for (int j = 0; j<3; j++) {
      for (int k = 0; k<3; k++) {
        for (int l = 0; l<3; l++) {
          hypo_L[i][j][k][l] = a1*kck[i][j][k][l] / 2 + a2*kdk[i][j][k][l] + a3*(pdk[i][j][k][l] + kdp[i][j][k][l]) + a4*pck[i][j][k][l] + a5*pdp[i][j][k][l];
        }
      }
    }
  }
  array_multiply(&hypo_L[0][0][0][0], &hypo_L[0][0][0][0], fs, 81);

  double hypo_Dsom[MDIM][MDIM];
  array_set(&hypo_Dsom[0][0], 0, MDIM*MDIM);
  double kpow = 1.7 + 3.9*sin(phic)*sin(phic);
  double sinphickpow = scalar_power(sin(phic), kpow);
  double sinphimkpow = scalar_power(sqrt(sin2phim), kpow);

  double TT[MDIM][MDIM];
  array_set(&TT[0][0], 0, MDIM*MDIM);
  matrix_ab(&T_str_star[0][0], &T_str_star[0][0], &TT[0][0], MDIM, MDIM, MDIM);
  double TTT[MDIM][MDIM];
  array_set(&TTT[0][0], 0, MDIM*MDIM);
  matrix_ab(&TT[0][0], &T_str_star[0][0], &TTT[0][0], MDIM, MDIM, MDIM);
  double cos3theta = -sqrt(6.)*trace(TTT) / scalar_power(trace(TT), 1.5);
  if (isotrop) cos3theta = -1;

  double Amult = 2. / 3 - sqrt(sqrt(sin2phim))*(cos3theta + 1) / 4.;

  for (int i = 0; i<MDIM; i++) {
    for (int j = 0; j<MDIM; j++) {
      hypo_Dsom[i][j] = -T_str_star[i][j] + kron_delta[i][j] * (sinphimkpow - sinphickpow) / (1 - sinphickpow)*Amult;
    }
  }
  double Dsomnorm = array_size(&hypo_Dsom[0][0], MDIM*MDIM);
  array_multiply(&hypo_Dsom[0][0], &hypo_Dsom[0][0], (1 / Dsomnorm), MDIM*MDIM);

  double A[3][3][3][3];
  array_set(&A[0][0][0][0], 0, 81);
  for (int i = 0; i<3; i++) {
    for (int j = 0; j<3; j++) {
      for (int k = 0; k<3; k++) {
        for (int l = 0; l<3; l++) {
          A[i][j][k][l] = hypo_L[i][j][k][l] + T[i][j] * kron_delta[k][l] / lambda;
        }
      }
    }
  }
  matrix_a4b(A, &hypo_Dsom[0][0], &hypo_N[0][0]);
  array_multiply(&hypo_N[0][0], &hypo_N[0][0], -fddivfdA, MDIM*MDIM);



  std:cout << "Tensor L" << std::endl;
  for (int i = 0; i<3; i++) {
    for (int j = 0; j<3; j++) {
      for (int k = 0; k<3; k++) {
        for (int l = 0; l<3; l++) {
          std::cout << hypo_L[i][j][k][l] << std::endl;
        }
      }
    }
  }

  std::cout << "Tensor N" << std::endl;
  for (int i = 0; i<3; i++) {
    for (int j = 0; j<3; j++) {
      std::cout << hypo_N[i][j] << std::endl;
    }
  }
}

int _tmain(int argc, _TCHAR* argv[])
{
  give_LN(25.0 / 180. * PI, 0.1, 0.01, 1.0, 0.2);
	return 0;
}

