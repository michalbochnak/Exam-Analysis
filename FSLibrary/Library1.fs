//
// My F# library for exam score analysis.
//
// Michal Bochnak, mbochn2
// U. of Illinois, Chicago
// CS 341, Fall 2017
// Project #04
//

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
    match L with
    | [] -> 0                       // base case
    | hd::tl -> 1 + (NumScores tl)  // recursive call


// 
// _FindMin
//
// Recursive helper function for FindMin
//
let rec _FindMin minSoFar L =
    match L with
    | [] -> minSoFar                        // base case
    | hd::tl -> if hd < minSoFar            
                then _FindMin hd tl         // hd < minSoFar
                else _FindMin minSoFar tl   // hd > minSoFar


//
// FindMin
//
// Recursively finds the min score in the list.
//
let rec FindMin L = 
    let firstEl = List.head L
    _FindMin firstEl (List.tail L)  // helper call


// 
// _FindMax
//
// Recursive helper function for FindMax
// Recursively finds the max score in the list
//
let rec _FindMax maxSoFar L =
    match L with
    | [] -> maxSoFar                      // base case
    | hd::tl -> if hd > maxSoFar          
                then _FindMax hd tl       // hd > maxSoFar
                else _FindMax maxSoFar tl // hd < maxSoFar


//
// FindMax
//
// Recursively finds the max score in the list.
//
let rec FindMax L = 
  let firstEl = List.head L
  _FindMax firstEl (List.tail L)  // helper call


//
// _Sum
//
// Recursively finds the sum of the list elements
//
let rec _Sum sum L = 
    match L with
    | [] -> 0                       // base case
    | hd::tl -> hd + (_Sum sum tl)  // recursive call


//
// Average
//
// Computes the average of a non-empty list of integers;
// the result is a real number (not an integer).
//
let Average L = 
  if ((List.length L) = 1)       // one element list
  then (float (List.head L))    // avg is the element
  else ((float (_Sum 0 L)) / (float (List.length L))) // sum / elements


//
// Median
//
// Computes the median of a non-empty list of integers;
// the result is a real number (not an integer) since the 
// median may be the average of 2 scores if the # of scores
// is even.
//
let Median L = 
  let tempL = List.sort L   // sort the list
  let index = ((List.length L) / 2)
  if ((List.length L) % 2) = 0  // even list
  then 
    let first = tempL.[index - 1]
    let second = tempL.[index]
    let median = ((float first) + (float second)) / (float 2)
    median
  else                        // odd list
    (float (tempL.[index]))


//
// StdDev
//
// Computes the standard deviation of a complete population
// defined by the integer list L.  Returns a real number.
//
let StdDev L = 
  let mean = Average L  // get average
  let tempL = List.map (fun x -> float(x)) L  // map to floats
  let diffs = List.map (fun x -> System.Math.Pow(x-mean, 2.0)) tempL // find difference
  let result = System.Math.Sqrt(List.average diffs) // square root diffs
  result                // return the answer


//
// Histogram
//
// Returns a list containing exactly 5 integers: [A;B;C;D;F].
// The integer A denotes the # of scores in L that fell in the
// range 90-100, inclusive.  B is the # of scores that fell in
// the range 80-89, inclusive.  C is the range 70-79, D is the
// range 60-69, and F is the range 0-59.
//
let Histogram L = // build the lists with ranges
  let As = List.filter (fun x -> x >= 90) L
  let Bs = List.filter (fun x -> (x <= 89 && x >= 80)) L
  let Cs = List.filter (fun x -> (x <= 79 && x >= 70)) L
  let Ds = List.filter (fun x -> (x <= 69 && x >= 60)) L
  let Fs = List.filter (fun x -> x <= 59) L

  // add leghts of the lists to the 'final'
  let final = [(List.length As); (List.length Bs); (List.length Cs);
              (List.length Ds); (List.length Fs)]
  final   // return list with lengths
  

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
let rec Trend L1 L2 L3 =
  match L1, L2, L3 with
  | [], [], []  ->  []    // base case
  | hd1::tl1, hd2::tl2, hd3::tl3  ->  if ((hd1 < hd2) && (hd2 < hd3)) then
                                        '+' :: (Trend tl1 tl2 tl3)
                                      elif ((hd1 > hd2) && (hd2 > hd3)) then
                                        '-' :: (Trend tl1 tl2 tl3)
                                      else 
                                        '=' :: (Trend tl1 tl2 tl3)
