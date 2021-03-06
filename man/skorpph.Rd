% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/skorpph.R
\name{skorpph}
\alias{skorpph}
\title{Desirable dietary pattern calculation}
\usage{
skorpph(data, wilayah = "Indonesia", baseline = 2000)
}
\arguments{
\item{data}{A data set of (\emph{n x 218})  (\emph{see} \strong{Details}).}

\item{wilayah}{An origin of the responden residence. 
(\emph{see} \strong{Details}).}

\item{baseline}{A baseline value of personal calory required.}
}
\value{
Function returns a vector with \emph{n} length indicates the  
index/ indices of the DDP per peson.
}
\description{
This function calculates the desirable dietary pattern (DDP).
}
\details{
The data set is an \emph{n x 218} data frame. The first column is
the name of the respondent. \code{wilayah} argument has "Indonesia" as the 
default, meaning that the DPP are calculated based on the national (Indonesia)
baseline. The other possible inputs for \code{wilayah} are "Aceh", "Sumut",
"Sumbar", "Riau", "KepRiau", "Jambi", "Sumsel", "Babel", "Bengkulu",
"Lampung", "Jakarta", "Jabar", "Banten", "Jateng", "DIY", "Jatim", "Bali",
"NTB", "NTT", "Kalbar", "Kalteng", "Kalsel", "Kaltim", "Kalut", "Sulut",
"Sulteng", "Sultra", "Sulsel", "Gorontalo", "Sulbar", "Maluku", "Malut",
"Papua", "Papbar". For \code{baseline} argument, it is 2000 as the default 
value because the minimal calory required in Indonesia is 2000 calory.
}
\examples{
#data simulation of 10 person
set.seed(2020)
n <- 10
matsim <- matrix(0, n, 218)
datsim <- as.data.frame(matsim)
datsim$V1 <- LETTERS[1:n]

#calory for boiled rice
datsim$V2 <- rnorm(n, 200, 50)
#calory for boiled egg
datsim$V73 <- rnorm(n, 60, 5)
#calory for fresh milk
datsim$V79 <- rnorm(n, 100, 10)
#calory for tomato
datsim$V93 <- rnorm(n, 19, 2)
#caloty for pineapple
datsim$V134 <- rnorm(n, 20, 2)

skorpph(datsim)

}
\references{
BKP, Kementan. 2017. Aplikasi Harmonisasi Analisis PPH Data
Susenas 2017. Badan Ketahanan Pangan Kementrian Pertanian.
}
\author{
Weksi Budiaji \cr Contact: \email{budiaji@untirta.ac.id}
}
