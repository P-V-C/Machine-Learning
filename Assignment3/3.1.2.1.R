#clear all variables from workspace
rm( list = ls() )

# here, set the directory parent to your "Data" directory
setwd( "~/github/StatML/Assignment3/" )

# read in the training and test data
sincTrain <- read.table( "./Data/sincTrain50.dt" )
sincTest <- read.table( "./Data/sincTest50.dt" )

# divide training and test data into inputs and outputs
inputs <- sincTrain$V1
outputs <- sincTrain$V2

inputsT <- sincTest$V1
outputsT <- sincTest$V2

# global variables to be used in functions - ugly - yes, but so is R ;)

# NN training parameters
bias <- 1
minW <- -0.01
maxW <- 0.01
LR <- 0.01

# activation funciton
actFunc <- function( a )
{
  
  a / ( 1 + abs( a )  )
  
}

# derivative of activation function
actFuncD <- function( a )
{
  
  1 / ( 1 + abs( a )  )^2
  
}

# return weights matrix of dimensions inps*nrns
makeWeights <- function( inps, nrns )
{
  
  matrix( runif( inps * nrns, minW, maxW ), inps, nrns )
  
}

# add a bias neuron to an input layer
makeLayerI <- function( input )
{
  
  c( bias, input )
  
}

# apply input weights to the inputs
applyWeightsI <- function( inputL, weightsI )
{
  
  weightsI[ 2, ] <- weightsI[ 2, ] * matrix( inputL[ 2 ], 1, length( weightsI[ 2, ] )  )
  weightsI[ 1, ] + weightsI[ 2, ]
  
}

# apply activation function to the hidden layer neurons' values
applyActF <- function( inputsH )
{
  
  sapply( inputsH, actFunc )
  
}

# get the forward-propagated output value - output layer
getOutput <- function( layerH, weightsO )
{
  
  sum( layerH * weightsO )
  
}

# back-propagation: calculate delta values for the hidden layer neurons
getDeltasH <- function( neuronsH, weightsO, deltaO )
{
  
  sapply( neuronsH, actFuncD ) * weightsO * matrix( deltaO, 1, length( neuronsH )  )
  
}

# back-propagation: calculate the delta derivatives of the hidden layer for inputs and update their sum
getDeltasHder <- function( input, deltasHder, deltasH )
{
  
  deltasHder <- deltasHder + ( matrix( input, 1, length( deltasHder ) ) * deltasH )
  
}

# back-propagation: calculate the delta derivatives of the hidden layer for biases and update their sum
getDeltasBder <- function( deltasHder, deltasH )
{
  
  deltasHder <- deltasHder + deltasH
  
}

# back-propagation: calculate the delta derivatives of the output layer for activated neurons and update their sum
getDeltasOder <- function( deltasOder, inputL, deltaO )
{
  
  deltasOder <- deltasOder + ( inputL * matrix( deltaO, 1, length( deltasOder ) ) )
  
}

# train the neural network
trainAndRunNN <- function( inps, outs, inpsT, hnrs, itrs )
{
  
  # initiate input and output weights
  weightsI <- makeWeights( 2, hnrs )
  weightsO <- makeWeights( 1, hnrs + 1 )
  
  # iterations and rms vectors for plotting rms
  its <- c()
  rms <- c()
  
  # train through itrs iterations
  for( itr in 1:itrs )
  {
    
    # make 0-filled 1*hnrs matrices for the hidden layer delta derivative sums
    deltasHderI <- matrix( 0, 1, hnrs )
    deltasHderB <- matrix( 0, 1, hnrs )
    
    # make 0-filled 1*(hnrs+1) matrices for the output layer delta derivative sums
    deltasOder <- matrix( 0, 1, hnrs + 1 )    

    # initialize delsqSum as 0 at each iteration for rms calculation purposes
    delsqSum <- 0
    
    # iterate through inputs
    for( inp in 1:length( inps ) )
    {

      # step 1: construct input layer by adding bias to input
      inputL <- makeLayerI( inps[ inp ] )

      # step 2: apply input weights to input and bias, sum them and store in hidden layer neurons
      inputsH <- applyWeightsI( inputL, weightsI  )

      # step 3: apply activation function to the values in the hidden layer neurons
      inputsHact <- applyActF( inputsH )
 
      # step 4: add a bias neuron to the hidden layer
      inputL <- makeLayerI( inputsHact )
     
      # step 5: apply hidden layer neuron values to output weights, sum the results and get output value
      outputL <- getOutput( inputL, weightsO )
   
      # step 6: back-propagation: get output layer delta - output-val - training-val
      deltaO <- outputL - outs[ inp ]

      # calculate the running sum of the squares of output deltas for rms
      delsqSum <- delsqSum + deltaO^2
    
      # step 7: back-propagation: calculate deltas for hidden layer using weighed inputs and output delta
      deltasH <- getDeltasH( inputsH, weightsO[ -1 ], deltaO )
     
      # step 8: back-propagation: calculate running sum of of hidden layer delta derivatives for inputs using the hidden deltas
      deltasHderI <- getDeltasHder( inps[ inp ], deltasHderI, deltasH )
      
      # step 9: back-propagation: calculate running sum of of hidden layer delta derivatives for biases using the hidden deltas
      deltasHderB <- getDeltasBder( deltasHderB, deltasH )
      
      # step 10: back-propagation: calculate running sum of of output layer delta derivatives using the activated hidden hidden values and output delta
      deltasOder <- getDeltasOder( deltasOder, inputL, deltaO )
      
    }
    
    # FINALLY: update the weights for:
      # the biases of the input layer
    weightsI[ 1, ] <- weightsI[ 1, ] - ( matrix( LR, 1, length( weightsI[ 1, ] ) ) * deltasHderB ) 
      # the inputs of the input layer
    weightsI[ 2, ] <- weightsI[ 2, ] - ( matrix( LR, 1, length( weightsI[ 2, ] ) ) * deltasHderI )
      # the hidden layer neuron outpus
    weightsO <- weightsO - ( matrix( LR, 1, length( weightsO ) ) * deltasOder )
    
    # calculate the rms
    rms <- append( rms, delsqSum / 2 )
    its <- append( its, itr )
    
    if ( itr %% 10000==0 ){ print(inps) } 

  }
 
  # plot the rms
  pdf('rms.pdf')
  plot( its, rms , ylab='RMS' , xlab='Iterations' )
  dev.off()
  runNN( inpsT, weightsI, weightsO )
  
}

# train the neural network
runNN <- function( inps, weightsI, weightsO )
{
  
  # initiate empty output vector
  theOutputs <- c()
  
  # iterate through inputs
  for( inp in 1:length( inps ) )
  {
    
    # step 1: construct input layer by adding bias to input
    inputL <- makeLayerI( inps[ inp ] )
    
    # step 2: apply input weights to input and bias, sum them and store in hidden layer neurons
    inputsH <- applyWeightsI( inputL, weightsI )
    
    # step 3: apply activation function to the values in the hidden layer neurons
    inputsHact <- applyActF( inputsH )
    
    # step 4: add a bias neuron to the hidden layer
    inputL <- makeLayerI( inputsHact )
    
    # step 5: apply hidden layer neuron values to output weights, sum the results and get output value
    outputL <- getOutput( inputL, weightsO )

    # step 6: add output to the outputs vector
    theOutputs <- append( theOutputs, outputL )
    
  }
  
  # plot the resulting graph
    pdf('output.pdf')
    plot( inps, theOutputs, xlab='x', ylab='y' )
    dev.off()
}
      
trainAndRunNN( inputs, outputs, inputsT, 20, 100000 )
