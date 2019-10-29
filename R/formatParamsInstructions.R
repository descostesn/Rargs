# This function prints instructions based on the expectedArgsDefinition list defined by user
# 
# Author: fenouil
###############################################################################


formatExpectedArgsInstructions=function(expectedArgsDefinition, headerText="Usage: Rscript `your_script.R` [arguments]\n\narguments:")
{
	message=paste("\n\n", headerText, sep="");

	for(currentExpectedArg in names(expectedArgsDefinition))
	{
		message=paste(message, "\n",currentExpectedArg," ARG :\n\t", expectedArgsDefinition[[currentExpectedArg]]$description, 
						ifelse(expectedArgsDefinition[[currentExpectedArg]]$mandatory,ifelse(is.null(expectedArgsDefinition[[currentExpectedArg]]$default),
																					"",
																					paste(" (Typical : ",expectedArgsDefinition[[currentExpectedArg]]$default,")",sep="" )), 
																				ifelse(is.null(expectedArgsDefinition[[currentExpectedArg]]$default),
																					" (Optional)", 
																					paste(" (Default : ",expectedArgsDefinition[[currentExpectedArg]]$default,")",sep="" ))), sep="");
	}
	message=paste(message, "\n\n", sep="");

	return(message);
}

# Define a synonym for compatibility
formatParamsInstructions=formatExpectedArgsInstructions;
