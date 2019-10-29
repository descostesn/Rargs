\name{getParams}
\alias{getParams}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Parsing command line parameters given to the current R script (Rscript `filename.R` -param1 value1...)}
\description{
  Retrieves and parses parameters from command line
}
\usage{
getParams(parametersDescription) # Deprecated
getArgs(expectedArgsDefinition)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expectedArgsDefinition}{A list with correct format describing expected command line arguments (see example).
	  This named list contains minimal information about type and names of expected parameters.
	  Elements names will define the expected parameters names to search in the command line.
	  All names can be considered as regular expressions (see conditional separator in example) that will be used to match command line string.
	  Each list element should be a sublist defining these parameters :
	  1. variableName: the name of the new variable to which the value will be affected in the global environment.
	  2. numeric: boolean defining if getParams should expect a numeric value. If true, a numeric conversion will be tried, raising an error if the parameter could not be converted correctly (sets postConversion as 'as.numeric').
	  3. mandatory: boolean defining if this argument will have to be specified by the user. An error is raised if the user doesn't give a value for a parameter marked as mandatory.
	  4. description: a (short) string describing the parameter (can be used to generate instructions or error message for final users)
	  5. default: a default value for the variable (see details for mandatory vs non-mandatory parameters behaviour)
	  6. postConversion: an optional function used to define the final 'type conversion' of the value. If specified, a conversion will be tried, raising an error if the parameter could not be converted correctly. Setting 'numeric' as 'TRUE' overrides this parameter by forcing it to 'as.numeric'. If nothing specified and numeric is false, one should expect to retrieve the value as a string.
  }
}
\details{
This function retrieves the values for each parameter put on the command line.
All succesfully processed parameters will be assigned to global environment.

SPECIAL VALUES
R special values ('NA' and 'NULL') are handled in a specific way.
R does not allow 'NULL' value(s) in a vector (NULL has length 0). As a consequence, any 'NULL' value in a vector will result in a NULL value affected to the variable, ignoring the other values (see chapter 'SEVERAL VALUES').
Any parameter (independently of its type) with value defined to 'NA' or 'NULL' will be affected the respective R special value.
This implies that a user CANNOT pass a STRING (character) with value "NA" or "NULL" to a script, these values being automaticaly interpreted as special values and assigned to variable. If one needs a character value "NA" or "NULL", he should consider to convert it as character (later in the script or using postConversion option 'as.character').
HOWEVER, since NULL is internally used to check for the definition of a default value, setting NULL as default value will be interpreted as 'no default value for this variable', quoting the "NULL" in the default values will allow to define a default value as NULL.
One should note that special values 'Inf' and '-Inf' are correctly handled by numeric conversion, and hence, can be used as numeric argument in command line.

NO VALUE
getParams cannot handle parameters with no value (consider a 'logical' TRUE or FALSE if needed).

SEVERAL VALUES
Everything before the first parameter on the command line will be ignored.
The library can handle several values (vector) for a parameter, space separated.

MANDATORY AND DEFAULT VALUES
The following politics are defined for mandatory argument with or without default value:
  - All mandatory parameters HAVE TO be specified by the final user, if a default value is
    specified by the developer it will just be marked as "typical values" in the instructions
  - Non mandatory parameter with a default value will only take default value if no value is specified by user
  - Non mandatory parameter without default value will not be assigned to the environment
  - Developper should declare default values directly in R style (i.e. 'c(3,4,5)' instead of '"3 4 5"'), not as expected to be in command line. This because default values are affected after command line parsing (unless the developper is sure that the postConversion function will handle it correctly. Should be ok in most cases, except when several values are used)
  - Use with care default values defined as "NULL", read SPECIAL VALUES chapter first
  

ERRORS
The function will raise an error while parsing command line arguments if :
  - A parameter name is defined more than once in the parameters definition list (see below)
  - A parameter is defined more than once at execution time
  - A parameter marked as mandatory is omitted
Remark about first condition: This test supposes that the developper specified simple names with eventual '|' separator(s), in case of complex regular expression, this test might not be able to find a redundant parameter name.

When (if) an error is raised, the function writes the expected parameters list (instructions) in the console.
}
\value{
  Parameters are directly assigned to global environment.
  Returns NULL.
}
\references{fenouil@ciml.univ-mrs.fr}
\author{Romain Fenouil}
\seealso{
  \code{\link[RIO]{"formatParamsInstructions"}}
}
\examples{
#### This list is an example for parameters specification
expectedArgsDefinition=list();
expectedArgsDefinition[["--inputFile|-i"]]=list(variableName="fileToRead", numeric=F, mandatory=T, description="Aligned file name");
expectedArgsDefinition[["--threshold|-t"]]=list(variableName="artefactThr", numeric=T, mandatory=T, description="Threshold for artefact removal", default=7000000);
expectedArgsDefinition[["--number|-n"]]=list(variableName="counter", numeric=T, mandatory=F, description="Number of experiments", default=1);
expectedArgsDefinition[["--logFile|-l"]]=list(variableName="logFilename", numeric=F, mandatory=F, description="Output log file name");
expectedArgsDefinition[["--GenerateWIG|-w"]]=list(variableName="WIGbool", numeric=F, mandatory=F, description="Create a wig file (TRUE|FALSE)", default=FALSE, postConversion=as.logical);


getArgs(expectedArgsDefinition);
}
\keyword{ misc }

