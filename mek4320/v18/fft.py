from __future__ import division
import os
import numpy
from numpy import linspace, exp, sqrt, pi
from numpy.fft import fft, ifft, fftfreq
from matplotlib import pyplot

def read_num_data():
    """
    Read the numX files with given numerical results
    """
    files_and_times = (("num1",   20.03125),
                       ("num3",   60.03125),
                       ("num5",  100.03125),
                       ("num10", 200.03125),
                       ("num20", 400.03125),
                       ("num40", 800.03125))
    
    data = {}
    for filename, time in files_and_times:
        # Load numeric result from file
        path = os.path.join('num-data2', filename)
        x, eta = numpy.loadtxt(path).T
        data[time] = x, eta
    return data

def initial_condition(x, L):
    return exp(-(x/L)**2)

def omega(k, q):
    "Dispersion relation"
    return (k**2 + q)**0.5

def stationary_phase_solution(t, q, L, x_min, x_max, N):
    """
    Finding an expression for eta from the stationary phase method
    """
    x = linspace(0.001, t-0.001, N)
    
    k0 = (q*x**2/(t**2 - x**2))**(1/2)
    Feta_0 = sqrt(pi)*L*exp(-L**2*k0**2/4)
    
    Y = lambda ksi: sqrt(q*ksi**2/(t**2 - x**2))
    ampl = L/sqrt(2*q*t)*Y(t)**(3/2) * exp(-(L*Y(x)/2)**2)
    eta_spm = ampl * exp(-1j*(t*Y(t) - x*Y(x) + pi/4)) 
    
    return x, eta_spm.real

def fourier_solution1(t, q, L, x_min, x_max, N):
    """
    Fourier solution where we do both FFT of eta_0 and
    inverse FFT to find the solution for a given time t
    """
    # Initial condition
    x = linspace(x_min, x_max, N)
    dx = x[1]-x[0]
    eta_0 = initial_condition(x, L)
    
    # Fourier transform of the initial condition
    Feta_0 = fft(eta_0)
    k = fftfreq(N, d=dx)*2*pi
    w = omega(k, q)
    
    # Inverse Fourier transform to find the solution, eta(x|t)
    eta_fft = ifft(Feta_0*exp(-1j*w*t))
    
    return x, eta_fft
    
def fourier_solution2(t, q, L, x_min, x_max, N):
    """
    Fourier solution where we do inverse FFT of the
    analyticaly Fourier transformed eta_0 to find the 
    solution for a given time t
    """
    # Initial condition
    x = linspace(x_min, x_max, N)
    dx = x[1]-x[0]
    
    # The analytical Fourier transform of the initial condition
    k = fftfreq(N, d=dx)*2*pi
    w = omega(k, q)
    Feta_0 = sqrt(pi)*L*exp(-L**2*k**2/4)/dx
    
    # Inverse Fourier transform to find the solution, eta(x|t)
    eta_fft = ifft(Feta_0*exp(-1j*w*t))
    
    # For some reason the signal is shifted ...
    ishift = numpy.argmin(abs(x))
    eta_fft = numpy.roll(eta_fft, ishift)
    
    return x, eta_fft

def plot_xlimits(x, eta, eps=1e-10):
    """
    Look at eta and find when it becomes approximately 0. This is 
    the maximum x position we need to plot
    
    Return x_min x_max which can be used to set the plot limits
    """
    for i in range(len(eta)):
        if abs(eta[i]) < eps and abs(eta[i+1]) < eps:
            return 0, x[i]
    
def main(): 
    # Read the given numerical data files
    num_data = read_num_data()
    
    # Define parameters
    q = 0.1
    L = 10
    
    # Define plot style
    pyplot.style.use('bmh')
    
    # Loop through the numerical data and produce comparison plots
    for t, (x_num, eta_num) in sorted(num_data.items()):
        x_max = x_num[-1]
        x_min = -x_max/4
        N = 100000
        
        # Stationary Phase Method
        x_spm, eta_spm = stationary_phase_solution(t, q, L, x_min, x_max, N)
        
        # FFT method 1
        x_fft1, eta_fft1 = fourier_solution1(t, q, L, x_min, x_max, N)
        
        # FFT method 2
        x_fft2, eta_fft2 = fourier_solution2(t, q, L, x_min, x_max, N)
        
        pyplot.figure()
        pyplot.title('t = %g' % t)
        pyplot.plot(x_spm, eta_spm, label='SFM')
        pyplot.plot(x_num, eta_num, label='Num.')
        pyplot.plot(x_fft1, eta_fft1.real, label='FFT1')
        pyplot.plot(x_fft2, eta_fft2.real, label='FFT2')
        
        pyplot.xlabel('$x$')
        pyplot.ylabel('$\eta$')
        pyplot.xlim(plot_xlimits(x_num, eta_num))
        pyplot.legend(loc='best')
        pyplot.tight_layout()
        pyplot.savefig(os.path.join('figures', 't%r.pdf' % t))
    
    pyplot.show()

if __name__ == '__main__':
    main()
