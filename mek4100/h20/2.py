# numerically solve differential equation u'' - u = eps*t*u
# and compare with perturbed solution

import numpy as np 
import matplotlib.pyplot as plt
import sympy as sp

t = np.linspace(0, 10, 10001)
u = np.zeros(len(t))

dt = t[1]-t[0]
eps = 0.04

#initial conditions
u[0] = 1
u[1] = 0.5*(t[0]*eps*dt**2 + dt**2 + 2 - 2*dt)

for i in range(1, len(t)-1):
    u[i+1] = u[i]*(eps*t[i]*dt**2 + dt**2 + 2) - u[i-1]

# perturbed solution
up = lambda t: np.exp(-t) + 1./8*eps*np.exp(-t)*(np.exp(2*t) - 1 - 2*t**2 - 2*t)

# taylor expansion of perturbed solution
x = sp.Symbol('x')
up_sympy = sp.exp(-x) + 1./8*eps*sp.exp(-x)*(sp.exp(2*x) - 1 - 2*x**2 - 2*x)
ut1 = sp.series(up_sympy, x, n=5).removeO()
ut2 = sp.series(up_sympy, x, n=10).removeO()
ut3 = sp.series(up_sympy, x, n=15).removeO()
ut4 = sp.series(up_sympy, x, n=20).removeO()

# convert sybolic expression to numeric function
ut1 = sp.lambdify(x, ut1, 'numpy')
ut2 = sp.lambdify(x, ut2, 'numpy')
ut3 = sp.lambdify(x, ut3, 'numpy')
ut4 = sp.lambdify(x, ut4, 'numpy')

# plot solutions
fig1 = plt.figure()
plt.plot(t, u, linewidth=2, label='Numerical solution')
plt.plot(t, up(t), linewidth=2, label='Perturbed solution')
plt.plot(t, ut1(t), linewidth=2, label='Taylor expansion of perturbation, 5 terms')
plt.plot(t, ut2(t), linewidth=2, label='Taylor expansion of perturbation, 10 terms')
plt.legend(loc='upper left')
plt.grid('on')
plt.title('Exercise 2, page 165 in Logan, dt=%g' %dt)
plt.xlabel('t'), plt.ylabel('u(t)')

fig2 = plt.figure()
plt.plot(t, u, linewidth=2, label='Numerical solution')
plt.plot(t, up(t), linewidth=2, label='Perturbed solution')
plt.plot(t, ut3(t), linewidth=2, label='Taylor expansion of perturbation, 15 terms')
plt.plot(t, ut4(t), linewidth=2, label='Taylor expansion of perturbation, 20 terms')
plt.legend(loc='upper left')
plt.grid('on')
plt.title('Exercise 2, page 165 in Logan, dt=%g' %dt)
plt.xlabel('t'), plt.ylabel('u(t)')

plt.show()

