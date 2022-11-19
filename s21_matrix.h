#ifndef SRC_S21_MATRIX_H_
#define SRC_S21_MATRIX_H_

#define SUCCESS 1
#define FAILURE 0
#define OK 0
#define INCORRECT 1
#define ERR 2

#define PRECISION 1E-7

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct matrix_struct {
  double** matrix;
  int rows;
  int columns;
} matrix_t;

int s21_create_matrix(int rows, int columns, matrix_t* result);
void s21_remove_matrix(matrix_t* A);
int s21_eq_matrix(matrix_t* A, matrix_t* B);
int s21_sum_matrix(matrix_t* A, matrix_t* B, matrix_t* result);
int s21_sub_matrix(matrix_t* A, matrix_t* B, matrix_t* result);
int s21_mult_number(matrix_t* A, double number, matrix_t* result);
int s21_mult_matrix(matrix_t* A, matrix_t* B, matrix_t* result);
int s21_check_matrix(matrix_t* A);
int s21_transpose(matrix_t* A, matrix_t* result);
int s21_calc_complements(matrix_t* A, matrix_t* result);
int s21_determinant(matrix_t* A, double* result);
void s21_minor(int I, int J, matrix_t* A, matrix_t* result);
int s21_inverse_matrix(matrix_t* A, matrix_t* result);
int ZeroDetermChecker(matrix_t* A);
int s21_zeroCnt(matrix_t* A);
int s21_swap(matrix_t* A, int* SwapCnt);
int findRowtoSwap(matrix_t* A);
void s21_gen_matrix(matrix_t* A);

#endif  // SRC_S21_MATRIX_H_
