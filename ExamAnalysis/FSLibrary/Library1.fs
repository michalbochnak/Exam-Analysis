module MyLibrary

#light

//
// InputScores
//
// Given the complete filepath to a text file of exam scores, 
// inputs the scores and returns them as a list of integers.
//
let InputScores filepath = 
  let L = [ for line in System.IO.File.ReadAllLines(filepath) -> line ]
  List.map (fun score -> System.Int32.Parse(score)) L


//
// NumScores
//
// Recursively counts the # of scores in the list.
//
let rec NumScores L = 
  -1


//
// FindMin
//
// Recursively finds the min score in the list.
//
let rec FindMin L = 
  -2


//
// FindMax
//
// Recursively finds the max score in the list.
//
let rec FindMax L = 
  -3


//
// Average
//
// Computes the average of a non-empty list of integers;
// the result is a real number (not an integer).
//
let Average L = 
  -4.0


//
// Median
//
// Computes the median of a non-empty list of integers;
// the result is a real number (not an integer) since the 
// median may be the average of 2 scores if the # of scores
// is even.
//
let Median L = 
  -5.0


//
// StdDev
//
// Computes the standard deviation of a complete population
// defined by the integer list L.  Returns a real number.
//
let StdDev L = 
  -6.0


//
// Histogram
//
// Returns a list containing exactly 5 integers: [A;B;C;D;F].
// The integer A denotes the # of scores in L that fell in the
// range 90-100, inclusive.  B is the # of scores that fell in
// the range 80-89, inclusive.  C is the range 70-79, D is the
// range 60-69, and F is the range 0-59.
//
let Histogram L = 
  [-1; -2; -3; -4; -5]


//
// Trend
//
// Trend is given 3 lists of integer scores:  L1, L2, L3.  The lists are 
// non-empty, and |L1| = |L2| = |L3|.  L1 are the scores for exam 01, L2
// are the scores for exam 02, and L3 are the scores for exam 03.  The
// lists are in "parallel", which means student i has their scores at 
// position i in each list.  Example: the first exam in each list denote
// the exams for student 0.
//
// Trend returns a new list R such that for each student, R contains a '+'
// if the exam scores were score1 < score2 < score3 --- i.e. the scores
// are trending upward.  R contains a '-' if score1 > score2 > score3, i.e.
// the scores are trending downward.  Otherwise R contains '=' (e.g. if
// score1 < score2 but then score2 > score3).  
//
let Trend L1 L2 L3 =
  [ for score in L1 -> '?']
