#' @title Titanic training data set
#'
#' @source Kaggle Titanic Competition: \url{https://www.kaggle.com/c/titanic/data}
#' @format Data frame of 712 observations and 12 features:
#' \describe{
#' \item{PassengerId}{id of the passenger}
#' \item{Survived}{dummy variable (0 if the passenger died / 1 if the passenger survived)}
#' \item{Pclass}{passenger class on the boat}
#' \item{Name}{name of the passenger}
#' \item{Sex}{male / female}
#' \item{Age}{age of the passenger}
#' \item{SibSp}{number of siblings/spouses aboard}
#' \item{Parch}{number of parents/children aboard}
#' \item{Ticket}{ticket no}
#' \item{Fare}{price of the ticket}
#' \item{Cabin}{location of the cabin on the boat}
#' \item{Embarked}{harbor city of boarding}
#' }
#' @examples
#' data(titanic_training)
"titanic_training"
