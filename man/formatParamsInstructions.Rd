\name{formatParamsInstructions}
\alias{formatParamsInstructions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summarize expected parameters}
\description{
  Creates a human readable string summarizing the expected arguments described in 'expectedArgsDefinition'
}
\usage{
formatParamsInstructions(parametersDescription) # Deprecated
formatExpectedArgsInstructions(expectedArgsDefinition) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expectedArgsDefinition}{A list with correct format describing expected commandline arguments (see getArgs help page)}
}
\details{
This functions gives formatted information about the parameters described in the 'parametersDescription' list argument.
It has been designed for internal use by getArgs function but can still be usefull for users.
}
\value{
  Character string.
}
\references{fenouil@ciml.univ-mrs.fr}
\author{Romain Fenouil}
\seealso{
  \code{\link[RIO]{"getParams"}}
}
\examples{
#### This list is an example for parameters specifications
expectedArgsDefinition=list();
expectedArgsDefinition[["--inputFile|-i"]]=list(variableName="fileToRead", numeric=F, mandatory=T, description="Aligned file name");
expectedArgsDefinition[["--threshold|-t"]]=list(variableName="artefactThr", numeric=T, mandatory=T, description="Threshold for artefact removal", default=7000000);
expectedArgsDefinition[["--number|-n"]]=list(variableName="counter", numeric=T, mandatory=F, description="Number of experiments", default=1);
expectedArgsDefinition[["--logFile|-l"]]=list(variableName="logFilename", numeric=F, mandatory=F, description="Output log file name");
expectedArgsDefinition[["--GenerateWIG|-w"]]=list(variableName="WIGbool", numeric=F, mandatory=F, description="Create a wig file (TRUE|FALSE)", default=FALSE, postConversion=as.logical);


getArgs(expectedArgsDefinition);
}
\keyword{ misc }

