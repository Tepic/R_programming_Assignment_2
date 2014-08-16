#Milan Tepic

#											ALGORITHM
#
# By calling the function makeCacheMatrix(matrix), programm will pass the matrix to this function
# which will then check if there is identical matrix in its (programm) cache. Furthermore,
# "tf" varible gets its value true/false (1/0) in accordance if there is identical matrix or not in cache.
# If there is identical matrix in the cache (which is list), then variable "position" will get the
# position of matrix in list that we looked for. After this checkin then,
# beside the inputed matrix, variable "tf" and "position" would be passed to the function cacheSolve.
# cacheSolve function will according to "tf" give the inverse matrix from the cache or do the inversion.
# After doing the inversion of the new matrix, then it would be listed in caches.

counter<-0
# counter of the position of specific matrix

cache_matrix<-list()
# cache for original matrix

cache_solution<-list()
# cache for inverse matrix of matrix from cache_matrix

makeCacheMatrix<-function(target)
{
	tf<-0			
	# initializing variable "tf" to false (0) as it was inputed new matrix, which don not exist in the cache
	
	position<-0
	# initializing position of the matrix to 0 in the list (where the matrix was found), as it was not found

	if(counter)
	{
		for(i in 1:counter)
			{
				checkin<-identical(target,cache_matrix[[i]])
				# checking if there is given matrix already calculated

				if(checkin)
				{
					# if it has already been calculated, checkin = 1 (TRUE)
					tf<-1
					position<-i
					# then variable position gets its position and
					# iteration of looking for same calculated matrix is over
					break
				}
			}
	}

	# after (not) finding the position of already calculated matrix
	# function cacheSolve is being called
	result<-cacheSolve(target,tf,position)
	result

	tf<-0
	position<-0
	# preparing function for new input
}

cacheSolve<-function(target,tf,position)
	{
		if(tf)
		{
			h<-c(cache_solution[position])
			solution<-h[[1]]
			# if it was calculated, then as solution it gets calculation from cache
		}

		else
		{
			solution<-solve(target)
			h<-counter
			counter<<-h+1
			cache_solution[[counter]]<<-solution
			cache_matrix[[counter]]<<-target

			# if it wasn not calculated, then it is being calculated here
			# and its solution is being stored in cache of solutions as well
			# as original matrix in cache of original matrix
		}
		casheSolve<-solution
		# returning the cached/calculated result
	}
