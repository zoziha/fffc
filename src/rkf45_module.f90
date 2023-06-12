module rkf45_module

    use fffc_kinds, only: rk => fffc_real_kind

    private
    public :: rkf45

    abstract interface
        !> `fcn` evaluates the derivative for the ODE.
        subroutine fcn(t, y, yp)
            import
            real(kind=rk), intent(in) :: t
            real(kind=rk), intent(in) :: y(:)
            real(kind=rk), intent(out) :: yp(:)
        end subroutine fcn
    end interface

contains
    !> rkf45 is primarily designed to solve non-stiff and mildly stiff
    !> differential equations when derivative evaluations are inexpensive.
    !> rkf45 should generally not be used when the user is demanding
    !> high accuracy.
    !> 在计算量不大的计算中，rkf45主要用于求解非刚性和轻微刚性的常微分方程。
    subroutine rkf45(f, neqn, y, t, tout, relerr, abserr, iflag, work, iwork)

        !     fehlberg fourth-fifth order runge-kutta method

        !     written by h.a.watts and l.f.shampine
        !                   sandia laboratories
        !                  albuquerque,new mexico

        ! abstract

        !    subroutine  rkf45  integrates a system of neqn first order
        !    ordinary differential equations of the form
        !             dy(i)/dt = f(t,y(1),y(2),...,y(neqn))
        !              where the y(i) are given at t .
        !    typically the subroutine is used to integrate from t to tout but it
        !    can be used as a one-step integrator to advance the solution a
        !    single step in the direction of tout.  on return the parameters in
        !    the call list are set for continuing the integration. the user has
        !    only to call rkf45 again (and perhaps define a new value for tout).
        !    actually, rkf45 is an interfacing routine which calls subroutine
        !    rkfs for the solution.  rkfs in turn calls subroutine  fehl which
        !    computes an approximate solution over one step.

        !    rkf45  uses the runge-kutta-fehlberg (4,5)  method described
        !    in the reference
        !    e.fehlberg , low-order classical runge-kutta formulas with stepsize
        !                 control , nasa tr r-315

        !    the performance of rkf45 is illustrated in the reference
        !    l.f.shampine,h.a.watts,s.davenport, solving non-stiff ordinary
        !                 differential equations-the state of the art ,
        !                 sandia laboratories report sand75-0182 ,
        !                 to appear in siam review.

        !    the parameters represent-
        !      f -- subroutine f(t,y,yp) to evaluate derivatives yp(i)=dy(i)/dt
        !      neqn -- number of equations to be integrated
        !      y(*) -- solution vector at t
        !      t -- independent variable
        !      tout -- output point at which solution is desired
        !      relerr,abserr -- relative and absolute error tolerances for local
        !            error test. at each step the code requires that
        !                 abs(local error) .le. relerr*abs(y) + abserr
        !            for each component of the local error and solution vectors
        !      iflag -- indicator for status of integration
        !      work(*) -- array to hold information internal to rkf45 which is
        !            necessary for subsequent calls. must be dimensioned
        !            at least  3+6*neqn
        !      iwork(*) -- integer array used to hold information internal to
        !            rkf45 which is necessary for subsequent calls. must be
        !            dimensioned at least  5

        !  first call to rkf45

        !    the user must provide storage in his calling program for the arrays
        !    in the call list  -      y(neqn) , work(3+6*neqn) , iwork(5)  ,
        !    declare f in an external statement, supply subroutine f(t,y,yp) and
        !    initialize the following parameters-

        !      neqn -- number of equations to be integrated.  (neqn .ge. 1)
        !      y(*) -- vector of initial conditions
        !      t -- starting point of integration , must be a variable
        !      tout -- output point at which solution is desired.
        !            t=tout is allowed on the first call only, in which case
        !            rkf45 returns with iflag=2 if continuation is possible.
        !      relerr,abserr -- relative and absolute local error tolerances
        !            which must be non-negative. relerr must be a variable while
        !            abserr may be a constant. the code should normally not be
        !            used with relative error control smaller than about 1.e-8 .
        !            to avoid limiting precision difficulties the code requires
        !            relerr to be larger than an internally computed relative
        !            error parameter which is machine dependent. in particular,
        !            pure absolute error is not permitted. if a smaller than
        !            allowable value of relerr is attempted, rkf45 increases
        !            relerr appropriately and returns control to the user before
        !            continuing the integration.
        !      iflag -- +1,-1  indicator to initialize the code for each new
        !            problem. normal input is +1. the user should set iflag=-1
        !            only when one-step integrator control is essential. in this
        !            case, rkf45 attempts to advance the solution a single step
        !            in the direction of tout each time it is called. since this
        !            mode of operation results in extra computing overhead, it
        !            should be avoided unless needed.

        !  output from rkf45

        !      y(*) -- solution at t
        !      t -- last point reached in integration.
        !      iflag = 2 -- integration reached tout. indicates successful retur
        !                   and is the normal mode for continuing integration.
        !            =-2 -- a single successful step in the direction of tout
        !                   has been taken. normal mode for continuing
        !                   integration one step at a time.
        !            = 3 -- integration was not completed because relative error
        !                   tolerance was too small. relerr has been increased
        !                   appropriately for continuing.
        !            = 4 -- integration was not completed because more than
        !                   3000 derivative evaluations were needed. this
        !                   is approximately 500 steps.
        !            = 5 -- integration was not completed because solution
        !                   vanished making a pure relative error test
        !                   impossible. must use non-zero abserr to continue.
        !                   using the one-step integration mode for one step
        !                   is a good way to proceed.
        !            = 6 -- integration was not completed because requested
        !                   accuracy could not be achieved using smallest
        !                   allowable stepsize. user must increase the error
        !                   tolerance before continued integration can be
        !                   attempted.
        !            = 7 -- it is likely that rkf45 is inefficient for solving
        !                   this problem. too much output is restricting the
        !                   natural stepsize choice. use the one-step integrator
        !                   mode.
        !            = 8 -- invalid input parameters
        !                   this indicator occurs if any of the following is
        !                   satisfied -   neqn .le. 0
        !                                 t=tout  and  iflag .ne. +1 or -1
        !                                 relerr or abserr .lt. 0.
        !                                 iflag .eq. 0  or  .lt. -2  or  .gt. 8
        !      work(*),iwork(*) -- information which is usually of no interest
        !                   to the user but necessary for subsequent calls.
        !                   work(1),...,work(neqn) contain the first derivatives
        !                   of the solution vector y at t. work(neqn+1) contains
        !                   the stepsize h to be attempted on the next step.
        !                   iwork(1) contains the derivative evaluation counter.

        !  subsequent calls to rkf45

        !    subroutine rkf45 returns with all information needed to continue
        !    the integration. if the integration reached tout, the user need onl
        !    define a new tout and call rkf45 again. in the one-step integrator
        !    mode (iflag=-2) the user must keep in mind that each step taken is
        !    in the direction of the current tout. upon reaching tout (indicated
        !    by changing iflag to 2),the user must then define a new tout and
        !    reset iflag to -2 to continue in the one-step integrator mode.

        !    if the integration was not completed but the user still wants to
        !    continue (iflag=3,4 cases), he just calls rkf45 again. with iflag=3
        !    the relerr parameter has been adjusted appropriately for continuing
        !    the integration. in the case of iflag=4 the function counter will
        !    be reset to 0 and another 3000 function evaluations are allowed.

        !    however,in the case iflag=5, the user must first alter the error
        !    criterion to use a positive value of abserr before integration can
        !    proceed. if he does not,execution is terminated.

        !    also,in the case iflag=6, it is necessary for the user to reset
        !    iflag to 2 (or -2 when the one-step integration mode is being used)
        !    as well as increasing either abserr,relerr or both before the
        !    integration can be continued. if this is not done, execution will
        !    be terminated. the occurrence of iflag=6 indicates a trouble spot
        !    (solution is changing rapidly,singularity may be present) and it
        !    often is inadvisable to continue.

        !    if iflag=7 is encountered, the user should use the one-step
        !    integration mode with the stepsize determined by the code or
        !    consider switching to the adams codes de/step,intrp. if the user
        !    insists upon continuing the integration with rkf45, he must reset
        !    iflag to 2 before calling rkf45 again. otherwise,execution will be
        !    terminated.

        !    if iflag=8 is obtained, integration can not be continued unless
        !    the invalid input parameters are corrected.

        !    it should be noted that the arrays work,iwork contain information
        !    required for subsequent integration. accordingly, work and iwork
        !    should not be altered.

        integer, intent(in) :: neqn
        real(kind=rk), intent(inout) :: y(neqn)
        real(kind=rk), intent(inout) :: t
        real(kind=rk), intent(in) :: tout
        integer, intent(inout) :: iflag, iwork(5)
        real(kind=rk), intent(inout) :: relerr, work(*)
        real(kind=rk), intent(in) :: abserr

        procedure(fcn) :: f

        integer :: k1, k2, k3, k4, k5, k6, k1m

        !     compute indices for the splitting of the work array

        k1m = neqn + 1
        k1 = k1m + 1
        k2 = k1 + neqn
        k3 = k2 + neqn
        k4 = k3 + neqn
        k5 = k4 + neqn
        k6 = k5 + neqn

        !     this interfacing routine merely relieves the user of a long
        !     calling list via the splitting apart of two working storage
        !     arrays. if this is not compatible with the users compiler,
        !     he must use rkfs directly.

        call rkfs(f, neqn, y, t, tout, relerr, abserr, iflag, &
                  work(1), work(k1m), work(k1), work(k2), work(k3), work(k4), work(k5), work(k6), work(k6 + 1), &
                  iwork(1), iwork(2), iwork(3), iwork(4), iwork(5))

        return
    end subroutine rkf45

    !> rkfs integrates a system of first order ordinary differential
    !> equations as described in the comments for rkf45 .
    subroutine rkfs(f, neqn, y, t, tout, relerr, abserr, iflag, &
                    yp, h, f1, f2, f3, f4, f5, savre, savae, &
                    nfe, kop, init, jflag, kflag)

        !     rkfs integrates a system of first order ordinary differential
        !     equations as described in the comments for rkf45 .
        !     the arrays yp,f1,f2,f3,f4,and f5 (of dimension at least neqn) and
        !     the variables h,savre,savae,nfe,kop,init,jflag,and kflag are used
        !     internally by the code and appear in the call list to eliminate
        !     local retention of variables between calls. accordingly, they
        !     should not be altered. items of possible interest are
        !         yp - derivative of solution vector at t
        !         h  - an appropriate stepsize to be used for the next step
        !         nfe- counter on the number of derivative function evaluations

        logical :: hfaild, output

        integer, intent(in) :: neqn
        real(kind=rk), intent(inout) :: y(neqn)
        real(kind=rk), intent(inout) :: t
        real(kind=rk), intent(in) :: tout
        integer, intent(inout) :: iflag
        real(kind=rk), intent(inout) :: relerr

        integer :: nfe, kop, init, jflag, kflag
        real(kind=rk) :: abserr, h, yp(neqn), f1(neqn), f2(neqn), f3(neqn), f4(neqn), f5(neqn), savre, savae

        procedure(fcn) :: f

        real(kind=rk) a, ae, dt, ee, eeoet, esttol, et, hmin, remin, rer, s, scale, tol, toln, twoeps, u26, ypk

        integer :: k, maxnfe, mflag

        !  remin is the minimum acceptable value of relerr.  attempts
        !  to obtain higher accuracy with this subroutine are usually
        !  very expensive and often unsuccessful.

        data remin/1.e-12_rk/

        !     the expense is controlled by restricting the number
        !     of function evaluations to be approximately maxnfe.
        !     as set, this corresponds to about 500 steps.

        data maxnfe/3000/

        !   here two constants emboding the machine epsilon is present
        !   twoesp is set to twice the machine epsilon while u26 is set
        !   to 26 times the machine epsilon

        data twoeps, u26/4.4e-16, 5.72e-15/

        !> Check input parameters
        if (neqn < 1 .or. relerr < 0.0_rk .or. abserr < 0.0_rk) goto 10
        mflag = abs(iflag)
        if ((mflag >= 1) .and. (mflag <= 8)) goto 20

        !     invalid input
10      iflag = 8
        return

        !     is this the first call
20      if (mflag == 1) goto 50

        !     check continuation possibilities

        if ((abs(t - tout) <= twoeps) .and. (kflag /= 3)) goto 10
        if (mflag /= 2) goto 25

        !     iflag = +2 or -2
        if (kflag == 3) goto 45
        if (init == 0) goto 45
        if (kflag == 4) goto 40
        if ((kflag == 5) .and. (abserr == 0.0_rk)) goto 30
        if ((kflag == 6) .and. (relerr <= savre) .and. (abserr <= savae)) goto 30
        goto 50

        !     iflag = 3,4,5,6,7 or 8
25      if (iflag == 3) goto 45
        if (iflag == 4) goto 40
        if ((iflag == 5) .and. (abserr > 0.0_rk)) goto 45

        !     integration cannot be continued since user did not respond to
        !     the instructions pertaining to iflag=5,6,7 or 8
30      stop

        !     reset function evaluation counter
40      nfe = 0
        if (mflag == 2) goto 50

        !     reset flag value from previous call
45      iflag = jflag
        if (kflag == 3) mflag = abs(iflag)

        !     save input iflag and set continuation flag value for subsequent
        !     input checking
50      jflag = iflag
        kflag = 0

        !     save relerr and abserr for checking input on subsequent calls
        savre = relerr
        savae = abserr

        !     restrict relative error tolerance to be at least as large as
        !     2*eps+remin to avoid limiting precision difficulties arising
        !     from impossible accuracy requests

        rer = twoeps + remin
        if (relerr >= rer) goto 55

        !     relative error tolerance too small
        relerr = rer
        iflag = 3
        kflag = 3
        return

55      dt = tout - t

        if (mflag == 1) goto 60
        if (init == 0) goto 65
        goto 80

        !     initialization --
        !                       set initialization completion indicator,init
        !                       set indicator for too many output points,kop
        !                       evaluate initial derivatives
        !                       set counter for function evaluations,nfe
        !                       evaluate initial derivatives
        !                       set counter for function evaluations,nfe
        !                       estimate starting stepsize

60      init = 0
        kop = 0

        a = t
        call f(a, y, yp)
        nfe = 1
        if (t /= tout) goto 65
        iflag = 2
        return

65      init = 1
        h = abs(dt)
        toln = 0.
        do k = 1, neqn
            tol = relerr*abs(y(k)) + abserr
            if (tol <= 0.) goto 70
            toln = tol
            ypk = abs(yp(k))
            if (ypk*h**5 > tol) h = (tol/ypk)**0.2_rk
70      end do
        if (toln <= 0.0_rk) h = 0.0_rk
        h = max(h, u26*max(abs(t), abs(dt)))
        jflag = sign(2, iflag)

        !     set stepsize for integration in the direction from t to tout

80      h = sign(h, dt)

        !     test to see if rkf45 is being severely impacted by too many
        !     output points

        if (abs(h) >= 2.0_rk*abs(dt)) kop = kop + 1
        if (kop /= 100) goto 85

        !     unnecessary frequency of output
        kop = 0
        iflag = 7
        return

85      if (abs(dt) > u26*abs(t)) goto 95

        !     if too close to output point,extrapolate and return

        do k = 1, neqn
            y(k) = y(k) + dt*yp(k)
        end do
        a = tout
        call f(a, y, yp)
        nfe = nfe + 1
        goto 300

        !     initialize output point indicator

95      output = .false.

        !     to avoid premature underflow in the error tolerance function,
        !     scale the error tolerances

        scale = 2.0_rk/relerr
        ae = scale*abserr

        !     step by step integration

100     hfaild = .false.

        !     set smallest allowable stepsize

        hmin = u26*abs(t)

        !     adjust stepsize if necessary to hit the output point.
        !     look ahead two steps to avoid drastic changes in the stepsize and
        !     thus lessen the impact of output points on the code.

        dt = tout - t
        if (abs(dt) >= 2.0_rk*abs(h)) goto 200
        if (abs(dt) > abs(h)) goto 150

        !     the next successful step will complete the integration to the
        !     output point

        output = .true.
        h = dt
        goto 200

150     h = 0.5_rk*dt

        !     core integrator for taking a single step

        !     the tolerances have been scaled to avoid premature underflow in
        !     computing the error tolerance function et.
        !     to avoid problems with zero crossings,relative error is measured
        !     using the average of the magnitudes of the solution at the
        !     beginning and end of a step.
        !     the error estimate formula has been grouped to control loss of
        !     significance.
        !     to distinguish the various arguments, h is not permitted
        !     to become smaller than 26 units of roundoff in t.
        !     practical limits on the change in the stepsize are enforced to
        !     smooth the stepsize selection process and to avoid excessive
        !     chattering on problems having discontinuities.
        !     to prevent unnecessary failures, the code uses 9/10 the stepsize
        !     it estimates will succeed.
        !     after a step failure, the stepsize is not allowed to increase for
        !     the next attempted step. this makes the code more efficient on
        !     problems having discontinuities and more effective in general
        !     since local extrapolation is being used and extra caution seems
        !     warranted.

        !     test number of derivative function evaluations.
        !     if okay,try to advance the integration from t to t+h

200     if (nfe <= maxnfe) goto 220

        !     too much work
        iflag = 4
        kflag = 4
        return

        !     advance an approximate solution over one step of length h

220     call fehl(f, neqn, y, t, h, yp, f1, f2, f3, f4, f5, f1)
        nfe = nfe + 5

        !     compute and test allowable tolerances versus local error estimates
        !     and remove scaling of tolerances. note that relative error is
        !     measured with respect to the average of the magnitudes of the
        !     solution at the beginning and end of the step.

        eeoet = 0.0_rk
        do k = 1, neqn
            et = abs(y(k)) + abs(f1(k)) + ae
            if (et > 0.0_rk) goto 240

            !       inappropriate error tolerance
            iflag = 5
            return

240         ee = abs((-2090.0_rk*yp(k) + &
                      (21970.0_rk*f3(k) - 15048.0_rk*f4(k))) + &
                     (22528.0_rk*f2(k) - 27360.0_rk*f5(k)))
            eeoet = max(eeoet, ee/et)
        end do

        esttol = abs(h)*eeoet*scale/752400.0_rk

        if (esttol <= 1.0_rk) goto 260

        !     unsuccessful step
        !                       reduce the stepsize , try again
        !                       the decrease is limited to a factor of 1/10

        hfaild = .true.
        output = .false.
        s = 0.1_rk
        if (esttol < 59049.0_rk) s = 0.9_rk/esttol**0.2_rk
        h = s*h
        if (abs(h) > hmin) goto 200

        !     requested error unattainable at smallest allowable stepsize
        iflag = 6
        kflag = 6
        return

        !     successful step
        !                        store solution at t+h
        !                        and evaluate derivatives there

260     t = t + h
        do k = 1, neqn
            y(k) = f1(k)
        end do
        a = t
        call f(a, y, yp)
        nfe = nfe + 1

        !                       choose next stepsize
        !                       the increase is limited to a factor of 5
        !                       if step failure has just occurred, next
        !                          stepsize is not allowed to increase

        s = 5.0_rk
        if (esttol > 1.889568e-4_rk) s = 0.9_rk/esttol**0.2_rk
        if (hfaild) s = min(s, 1.0_rk)
        h = sign(max(s*abs(h), hmin), h)

        !     end of core integrator

        !     should we take another step

        if (output) goto 300
        if (iflag > 0) goto 100

        !     integration successfully completed

        !     one-step mode
        iflag = -2
        return

        !     interval mode
300     t = tout
        iflag = 2
        return

    end subroutine rkfs

    !> fehl integrates a system of neqn first order
    !> ordinary differential equations of the form
    !>          dy(i)/dt=f(t,y(1),---,y(neqn))
    !> where the initial values y(i) and the initial derivatives
    !> yp(i) are specified at the starting point t.
    subroutine fehl(f, neqn, y, t, h, yp, f1, f2, f3, f4, f5, s)

        !    fehl advances the solution over the fixed step h and returns
        !    the fifth order (sixth order accurate locally) solution
        !    approximation at t+h in array s(i).
        !    f1,---,f5 are arrays of dimension neqn which are needed
        !    for internal storage.
        !    the formulas have been grouped to control loss of significance.
        !    fehl should be called with an h not smaller than 13 units of
        !    roundoff in t so that the various independent arguments can be
        !    distinguished.

        procedure(fcn) :: f
        integer, intent(in) :: neqn
        real(kind=rk) y(neqn), t, h, yp(neqn), f1(neqn), f2(neqn), f3(neqn), f4(neqn), f5(neqn), s(neqn)

        real(kind=rk) ch
        integer k

        ch = h/4.0_rk
        do k = 1, neqn
            f5(k) = y(k) + ch*yp(k)
        end do
        call f(t + ch, f5, f1)

        ch = 3.0_rk*h/32.0_rk
        do k = 1, neqn
            f5(k) = y(k) + ch*(yp(k) + 3.0_rk*f1(k))
        end do
        call f(t + 3.0_rk*h/8.0_rk, f5, f2)

        ch = h/2197.0_rk
        do k = 1, neqn
            f5(k) = y(k) + ch*(1932.0_rk*yp(k) + (7296.0_rk*f2(k) - 7200.0_rk*f1(k)))
        end do
        call f(t + 12.0_rk*h/13.0_rk, f5, f3)

        ch = h/4104.0_rk
        do k = 1, neqn
            f5(k) = y(k) + ch*((8341.0_rk*yp(k) - 845.0_rk*f3(k)) + &
                               (29440.0_rk*f2(k) - 32832.0_rk*f1(k)))
        end do
        call f(t + h, f5, f4)

        ch = h/20520.0_rk
        do k = 1, neqn
            f1(k) = y(k) + ch*((-6080.0_rk*yp(k) + &
                                (9295.0_rk*f3(k) - 5643.0_rk*f4(k))) &
                               + (41040.0_rk*f1(k) - 28352.0_rk*f2(k)))
        end do
        call f(t + h/2.0_rk, f1, f5)

        !> compute approximate solution at t+h

        ch = h/7618050.0_rk
        do k = 1, neqn
            s(k) = y(k) + ch*((902880.0_rk*yp(k) + (3855735.0_rk*f3(k) - &
                                                                 1371249.0_rk*f4(k))) &
                              + (3953664.0_rk*f2(k) + 277020.0_rk*f5(k)))
        end do

        return
    end subroutine fehl

end module rkf45_module
