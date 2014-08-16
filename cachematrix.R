#Milan Tepic

#Algorithm
# By calling the function makeCacheMatrix(matrix), programm will pass the matrix to this function
# which will then check if there is identical matrix in its (programm) cache. Furthermore,
# "tf" varible gets its value true/false (1/0) in accordance if there is identical matrix or not in cache.
# If there is identical matrix in the cache (which is list), then variable "position" will get the
# position of matrix in list that we looked for. After this checkin then,
# beside the inputed matrix, variable "tf" and "position" would be passed to the function cacheSolve.
# cacheSolve function will according to "tf" give the inverse matrix from the cache or do the inversion.
# After doing the inversion of the new matrix, then it would be listed in caches.

makeCacheMatrix<-function(target)
{
	tf<-0		#initializing variable "tf" to false (0) as it was input new matrix, which don not exist in the cache
	position<-0 #initializing found position of the matrix to 0 in the list, as it was not found

	initialize<-function()
		{
			counter<-0
			cache_matrix<-list()
			cache_solution<-list()
			checkin<-matrix(,0,0)
		}
	
	cacheSolve<-function(target,tf,position)
			{
				if(tf)
				{
					h<-c(cache_solution[position])
					solution<-h[[1]]
					#message("Uzima iz kesa")
				}

				else
				{
					solution<-solve(target)
					h<-counter
					counter<<-h+1
					cache_solution[[counter]]<<-solution
					cache_matrix[[counter]]<<-target
					#message("Ne uzima iz kesa")
				}
				casheSolve<-solution
			}

	if(!first_time_called_function)
	{
		initialize()
		first_time_called_function<-1
	}

	if(counter)
	{
		for(i in 1:counter)
			{
				checkin[i]<<-identical(target,cache_matrix[[i]])

				if(checkin[i])
				{
					tf<-1
					position<-i
					break
				}
			}
	}

	finale<-cacheSolve(target,tf,position)
	print(finale)

	tf<-0
	position<-0
}
