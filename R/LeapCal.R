#' Leap-year Calendar
#'
#' @description A leap-year calendar written in C++ and implemented in R with Rcpp. Just a trial in using Rcpp.
#'
#' @param year year, such as 2016.
#' @param month month, such as 08 or 8.
#'
#' @importFrom Rcpp sourceCpp
#'
#' @author Yujie Liu
#' @export



LeapCal <- function(year, month) {

  if (year %% 4 != 0 || year %% 100 == 0) stop("Not a leap year!")
  if (month < 1 || month > 12) stop("Month error!")

  calendarcpp <-'
#include <iostream>
using namespace std;

// [[Rcpp::export]]
void leapcal(int iYear, int iMonth)
{
	int iWeek, iDay, n, j;

	cout << "\tYear: " << iYear << "\t\tMonth: " << iMonth << endl;
	cout << "Sun\tMon\tTue\tWed\tThu\tFri\tSat" << endl;

	switch (iMonth)
	{
		case 1: n = 31; iYear--; iMonth = 13; break;
		case 3: case 5: case 7: case 8: case 10: case 12: n = 31; break;
		case 4: case 6: case 9: case 11: n = 30; break;
		case 2: n = 29; iYear--; iMonth = 14; break;
	}

	int c = iYear / 100, y = iYear % 100;

	iWeek = ((c/4)-2*c+y+(y/4)+(26*(iMonth+1)/10)) % 7;
	for (j = 1; j < iWeek + 1; j++)
		cout << "\t";

	for (iDay = 1; iDay <= n; iDay++)
	{
		iWeek = ((c/4)-2*c+y+(y/4)+(26*(iMonth+1)/10)+iDay-1) % 7;
		cout << iDay << "\t";
		if (iWeek == 6)
			cout << endl;
	}

	cout << endl;

}'

  sourceCpp(code = calendarcpp)
  leapcal(iYear = year, iMonth = month)

}
