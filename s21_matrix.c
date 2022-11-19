#include "s21_matrix.h"

int s21_create_matrix(int rows, int columns, matrix_t *result) {
  int returnable = OK;
  if (rows > 0 && columns > 0) {
    result->matrix = calloc((size_t)rows, sizeof(double *));
    if (result->matrix != NULL) {
      result->columns = columns;
      result->rows = rows;
      for (int i = 0; i < rows; i++) {
        result->matrix[i] = calloc((size_t)columns, sizeof(double));
        if (result->matrix[i] == NULL) {
          s21_remove_matrix(result);
          returnable = INCORRECT;
          break;
        }
      }
    } else {
      returnable = INCORRECT;
    }
  } else {
    result->matrix = NULL;
    returnable = INCORRECT;
  }
  return returnable;
}

void s21_remove_matrix(matrix_t *A) {
  if (A->matrix) {
    for (int i = 0; i < A->rows; i++) {
      free(A->matrix[i]);
      A->matrix[i] = NULL;
    }
    free(A->matrix);
    A->columns = 0;
    A->rows = 0;
    A->matrix = NULL;
  }
}

int s21_eq_matrix(matrix_t *A, matrix_t *B) {
  int returnable = SUCCESS;
  if (A->columns == B->columns && A->rows == B->rows) {
    for (int i = 0; i < A->rows; i++) {
      for (int j = 0; j < A->columns; j++) {
        double diff = A->matrix[i][j] - B->matrix[i][j];
        if (fabs(diff) > PRECISION) {
          returnable = FAILURE;
          break;
        }
      }
      if (returnable == FAILURE) break;
    }
  } else {
    returnable = FAILURE;
  }
  return returnable;
}

int s21_sub_matrix(matrix_t *A, matrix_t *B, matrix_t *result) {
  int error_code = OK;
  if (A != NULL && B != NULL) {
    if (A->rows != B->rows || A->columns != B->columns) {
      error_code = ERR;
    } else {
      error_code = s21_create_matrix(A->rows, A->columns, result);
      if (error_code == OK) {
        for (int i = 0; i < A->rows; i++) {
          for (int j = 0; j < A->columns; j++)
            result->matrix[i][j] = A->matrix[i][j] - B->matrix[i][j];
        }
      }
    }
  } else {
    error_code = INCORRECT;
  }
  return error_code;
}

int ZeroDetermChecker(matrix_t *A) {
  int ZeroColumnCounter = 0;
  int ZeroRowCounter = 0;
  for (int i = 0; i < A->rows; i++) {
    for (int j = 0; j < A->columns; j++) {
      if (A->matrix[j][i] == 0) ZeroColumnCounter++;
      if (A->matrix[i][j] == 0) ZeroRowCounter++;
    }
    if (ZeroColumnCounter == A->rows || ZeroRowCounter == A->columns) {
      break;
    } else {
      ZeroColumnCounter = 0;
      ZeroRowCounter = 0;
    }
  }
  return (ZeroColumnCounter == A->rows || ZeroRowCounter == A->columns) ? 1 : 0;
}

int s21_sum_matrix(matrix_t *A, matrix_t *B, matrix_t *result) {
  int error_code = OK;
  if (A != NULL && B != NULL) {
    if (A->rows != B->rows || A->columns != B->columns) {
      error_code = ERR;
    } else {
      error_code = s21_create_matrix(A->rows, A->columns, result);
      if (error_code == OK) {
        for (int i = 0; i < A->rows; i++) {
          for (int j = 0; j < A->columns; j++)
            result->matrix[i][j] = A->matrix[i][j] + B->matrix[i][j];
        }
      }
    }
  } else {
    error_code = INCORRECT;
  }
  return error_code;
}

int s21_mult_number(matrix_t *A, double number, matrix_t *result) {
  int returnable = OK;
  if (A->matrix != NULL) {
    returnable = s21_create_matrix(A->rows, A->columns, result);
    if (returnable == OK) {
      for (int i = 0; i < A->rows; i++) {
        for (int j = 0; j < A->columns; j++)
          result->matrix[i][j] = A->matrix[i][j] * number;
      }
    }
  } else {
    returnable = INCORRECT;
  }
  return returnable;
}

int s21_mult_matrix(matrix_t *A, matrix_t *B, matrix_t *result) {
  int returnable = OK;
  if (s21_check_matrix(A) == OK && s21_check_matrix(B) == OK) {
    if (A->columns == B->rows) {
      int returnable = s21_create_matrix(A->rows, B->columns, result);
      if (returnable == OK) {
        for (int i = 0; i < A->rows; i++) {
          for (int j = 0; j < B->columns; j++) {
            for (int k = 0; k < A->columns; k++) {
              result->matrix[i][j] += A->matrix[i][k] * B->matrix[k][j];
            }
          }
        }
      }
    } else {
      returnable = ERR;
    }
  } else {
    returnable = INCORRECT;
  }
  return returnable;
}

int s21_check_matrix(matrix_t *A) {
  int error_code = OK;
  if (A == NULL || A->matrix == NULL || A->rows < 1 || A->columns < 1) {
    /* матрицы не существует || матрица не создана || ... */
    error_code = ERR;
  }
  return error_code;
}

int s21_transpose(matrix_t *A, matrix_t *result) {
  int returnable = s21_check_matrix(A);
  if (returnable == OK) {
    returnable = s21_create_matrix(A->columns, A->rows, result);
    if (returnable == OK) {
      for (int i = 0; i < A->rows; i++) {
        for (int j = 0; j < A->columns; j++) {
          result->matrix[j][i] = A->matrix[i][j];
        }
      }
    }
  }
  return returnable;
}

int s21_calc_complements(matrix_t *A, matrix_t *result) {
  int returnable = s21_check_matrix(A);
  if (returnable == OK) {
    if (A->rows == A->columns) {
      returnable = s21_create_matrix(A->rows, A->columns, result);
      if (returnable == OK) {
        if (A->rows == 1) {
          result->matrix[0][0] = A->matrix[0][0];
        } else if (A->rows == 2) {  // check-this
          matrix_t forMinor;
          s21_create_matrix(1, 1, &forMinor);
          for (int i = 0; i < A->rows; i++) {
            for (int j = 0; j < A->columns; j++) {
              s21_minor(i, j, A, &forMinor);
              result->matrix[i][j] = forMinor.matrix[0][0] * pow(-1, (i + j));
            }
          }
          s21_remove_matrix(&forMinor);
        } else {
          matrix_t forMinor;
          s21_create_matrix((A->rows) - 1, (A->columns) - 1, &forMinor);
          double res = 0;
          for (int i = 0; i < A->rows; i++) {
            for (int j = 0; j < A->columns; j++) {
              s21_minor(i, j, A, &forMinor);
              s21_determinant(&forMinor, &res);
              result->matrix[i][j] = res * pow(-1, (i + j));
              res = 0;
            }
          }
          s21_remove_matrix(&forMinor);
        }
      }
    } else {
      returnable = ERR;
      result->matrix = NULL;
      result->rows = 0;
      result->columns = 0;
    }
  }
  return returnable;
}

/* функция находит и записывает минор одного элемента матрицы A в заранее
созданный result нужного размера */
void s21_minor(int I, int J, matrix_t *A, matrix_t *result) {
  int resI = 0;
  int resJ = 0;
  for (int i = 0; i < A->rows; i++) {
    for (int j = 0; j < A->columns; j++) {
      if (i != I && j != J) {
        result->matrix[resI][resJ] = A->matrix[i][j];
        if (resJ < (result->columns) - 1) {
          resJ++;
        } else {
          resJ = 0;
          resI++;
        }
      }
    }
  }
}

int s21_zeroCnt(matrix_t *A) {
  int zero_count = 0;
  for (int i = 0; i < A->rows; i++)
    if (A->matrix[i][i] == 0) zero_count++;
  return zero_count;
}

int s21_determinant(matrix_t *A, double *result) {
  int returnable = s21_check_matrix(A);
  if (returnable == OK) {
    if (A->columns == A->rows) {
      if (A->rows == 1) {
        *(result) = A->matrix[0][0];
      } else if (A->rows == 2) {
        *(result) = (A->matrix[0][0] * A->matrix[1][1]) -
                    (A->matrix[0][1] * A->matrix[1][0]);
      } else if (A->rows >= 3) {
        if (ZeroDetermChecker(A) == 1) {
          *(result) = 0;
        } else {
          int SwapCnt = 0;
          while (s21_zeroCnt(A) != 0) s21_swap(A, &SwapCnt);
          for (int i = 0; i < A->rows; i++) {
            for (int j = 0; j < A->columns; j++) {
              if (i > j) {
                if (fabs(A->matrix[i][j] - 0.000000) > PRECISION) {
                  double coef = 0.0;
                  if (j == 0) {
                    coef = (A->matrix[i][0] / A->matrix[0][0]) * (-1);
                    for (int elem = 0; elem < A->columns; elem++) {
                      A->matrix[i][elem] += (A->matrix[0][elem] * coef);
                    }
                  } else {
                    coef = (A->matrix[i][j] / A->matrix[i - 1][j]) * (-1);
                    for (int elem = j; elem < A->columns; elem++) {
                      A->matrix[i][elem] += (A->matrix[i - 1][elem] * coef);
                    }
                  }
                }
              }
            }
          }
          // следующие два цикла выполняют перемножение главной диагонали
          *(result) = 1;
          for (int a = 0; a < A->rows; a++) {
            for (int b = 0; b < A->columns; b++) {
              if (a == b) {
                *(result) *= A->matrix[a][b];
              }
            }
          }
          if (SwapCnt % 2 != 0) *(result) *= (-1);
        }
      }
    } else {
      returnable = ERR;
    }
  }
  return returnable;
}

int s21_swap(matrix_t *A, int *SwapCnt) {
  double *buffer = calloc(A->columns, sizeof(double));
  int RowToSwap = 0;
  for (int i = 0; i < A->rows; i++) {
    if (A->matrix[i][i] == 0) {
      RowToSwap = findRowtoSwap(A);
      for (int j = 0; j < A->columns; j++) {
        buffer[j] = A->matrix[RowToSwap][j];
        A->matrix[RowToSwap][j] = A->matrix[i][j];
        A->matrix[i][j] = buffer[j];
      }
      (*SwapCnt)++;
    }
  }
  free(buffer);
  return *SwapCnt;
}

int findRowtoSwap(matrix_t *A) {
  int NoZeroCnt = 0;
  int i = 0;
  for (; i < A->rows; i++) {
    for (int j = 0; j < A->columns; j++) {
      if (A->matrix[i][j] != 0) NoZeroCnt++;
    }
    if (NoZeroCnt == A->columns)
      break;
    else
      NoZeroCnt = 0;
  }
  return i;
}

int s21_inverse_matrix(matrix_t *A, matrix_t *result) {
  int returnable = s21_check_matrix(A);
  if (returnable == OK) {
    if (A->columns == A->rows) {
      // returnable = s21_create_matrix(A->rows, A->columns, result);
      if (returnable == OK) {
        double res = 0;
        matrix_t A_copy;
        s21_create_matrix(A->rows, A->columns, &A_copy);
        for (int i = 0; i < A->rows; i++) {
          for (int j = 0; j < A->columns; j++) {
            A_copy.matrix[i][j] = A->matrix[i][j];
          }
        }
        s21_determinant(&A_copy, &res);
        if (fabs(res - 0.000000) > PRECISION) {
          matrix_t transpose;
          s21_transpose(A, &transpose);
          s21_calc_complements(&transpose, result);
          for (int i = 0; i < A->rows; i++) {
            for (int j = 0; j < A->columns; j++) {
              result->matrix[i][j] *= (1 / res);
            }
          }
          s21_remove_matrix(&transpose);
          s21_remove_matrix(&A_copy);
        } else {
          s21_remove_matrix(&A_copy);
          returnable = ERR;
        }
      }
    } else {
      returnable = ERR;
    }
  }
  return returnable;
}

// void s21_gen_matrix(matrix_t *A) {
//   double num = 1;
//   for (int i = 0; i < A->rows; i++) {
//     for (int j = 0; j < A->columns; j++, num++) {
//       A->matrix[i][j] = num;
//     }
//   }
// }

// int main() {
//   const int size = 3;
//   matrix_t m = {0};
//   s21_create_matrix(size, size, &m);
//   m.matrix[0][0] = 1;
//   m.matrix[0][1] = 2;
//   m.matrix[0][2] = 3;
//   m.matrix[1][1] = 4;
//   m.matrix[1][2] = 2;
//   m.matrix[2][0] = 5;
//   m.matrix[2][1] = 2;
//   m.matrix[2][2] = 1;

//   matrix_t expected = {0};
//   s21_create_matrix(size, size, &expected);
//   expected.matrix[0][1] = 10;
//   expected.matrix[0][2] = -20;
//   expected.matrix[1][0] = 4;
//   expected.matrix[1][1] = -14;
//   expected.matrix[1][2] = 8;
//   expected.matrix[2][0] = -8;
//   expected.matrix[2][1] = -2;
//   expected.matrix[2][2] = 4;

//   matrix_t res = {0};
//   s21_calc_complements(&m, &res);

//   if (s21_eq_matrix(&expected, &res) == 1) printf("YES\n");
//   s21_remove_matrix(&m);
//   s21_remove_matrix(&res);
//   s21_remove_matrix(&expected);
//   return 0;
// }

// int main() {
//   matrix_t test, result;
//   s21_create_matrix(3, 2, &test);
//   int result_status;
//   s21_gen_matrix(&test);
//   result_status = s21_calc_complements(&test, &result);
//   if (result_status == 2) printf("YES\n");
//   s21_remove_matrix(&test);
//   s21_remove_matrix(&result);
//   return 0;
// }
