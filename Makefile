CC := gcc
CFLAGS := -std=c11 -ggdb -Wall -Werror -Wextra
TST_LIBS := -lcheck -lm -lpthread
TDIR = tests
EXEDIR = testrun/

all: clean gcov_report

clean: 
	@rm -rf *.a *.o *.gcda *.gcno test gcov_test *.info *.dSYM *.txt report *cfg *.out

test:  s21_matrix_test.o s21_matrix.a
	@clear
	@$(CC) $(CFLAGS) s21_matrix_test.o s21_matrix.a $(TST_LIBS) -o test
	@./test

s21_matrix_test.o: s21_matrix_test.c
	@$(CC) $(CFLAGS) -c s21_matrix_test.c -o s21_matrix_test.o

s21_matrix.o: s21_matrix.c
	@$(CC) $(CFLAGS) -c s21_matrix.c -o s21_matrix.o

s21_matrix.a: s21_matrix.o
	@ar rc s21_matrix.a s21_matrix.o
	@ranlib s21_matrix.a

gcov_test: s21_matrix_test.c s21_matrix.a
	@$(CC) $(CFLAGS) --coverage s21_matrix_test.c s21_matrix.c s21_matrix.a $(TST_LIBS) -o gcov_test

gcov_test.gcda: gcov_test
	@chmod +x *
	@./gcov_test

gcov_test.info: gcov_test gcov_test.gcda
	@lcov -t "gcov_test" -o gcov_test.info --no-external -c -d .

gcov_report: clean gcov_test.info
	@genhtml -o report/ gcov_test.info
	@open report/index.html

report_open: gcov_report
	@open report/index.html

cpplint:
	@cp ../materials/linters/CPPLINT.cfg ./
	@python3 ../materials/linters/cpplint.py *.c *.h
	@rm CPPLINT.cfg

cppcheck:
	@cppcheck *.c *.h

check: cpplint cppcheck

valgrind:
	CK_FORK=no valgrind --vgdb=no --leak-check=full --show-leak-kinds=all --track-origins=yes --verbose --log-file=RESULT_VALGRIND.txt ./test

leaks: test
	CK_FORK=no leaks --atExit -- ./test
	@make clean