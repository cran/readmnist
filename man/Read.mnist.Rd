\name{Read.mnist}
\alias{Read.mnist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read MNIST Dataset
}
\description{
read and arrange informations from MNIST dataset.
}
\usage{
Read.mnist(filename)
}
\arguments{
  \item{filename}{
filename of a MNIST dataset (example: "t10k-images.idx3-ubyte")
}
}
\details{
It will return a list (Info) including all necessary informations.
If it's a label file, Info will contain : $magic_number $labels $number $mark(the style of dataset);
If it's a image file, Info will contain : $ magic_number $pic(each row is a image) $nrow $ncol $number $mark(the style of dataset);
And if it's other invalid file, Info will be empty, and the function will print error messages.
}
\value{
  \item{magic_number}{the magic_number of file}
  \item{labels}{the labels information from the dataset(if it's a label file)}
  \item{pic}{the images from the dataset, and each row is one image.(if it's a image file)}
  \item{nrow}{the height of a image(if it's a image file)}
  \item{ncol}{the width of a image(if it's a image file)}
  \item{number}{the number of the dataset}
item{mark}{the remark of the Info}
}
\references{
https://stackoverflow.com/questions/21521571/how-to-read-mnist-database-in-r
}
\author{
Jiang Junfeng
}
\examples{
  Info <- Read.mnist(system.file("exdata/t10k-labels.idx1-ubyte", package="readmnist"))
}
