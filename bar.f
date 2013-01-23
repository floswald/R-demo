      subroutine bar(n, x)

      integer n
      double precision x(n)
      integer i

      do 100 i = 1, n
          x(i) = x(i) ** 2 
  100 continue

      end

