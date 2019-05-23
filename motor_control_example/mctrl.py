# ======================================================================
# File name      : mctrl.xml
# Purpose        : Reference code for acceleration- and speed-limited
#                  motor control algorithm, which iteratively computes
#                  motor speed for discrete time steps. The algorithm
#                  autonomously accelerates and decelerates the motor
#                  in order to rotate the motor and then stop precisely
#                  after the specified distance.
# Authors        : Jakob Lechner (LEJ)
# ======================================================================
# Copyright      : The ownership and copyright of this code belong
#                  to RUAG Space GmbH and it must not be
#                  disclosed, copied, altered or used without written
#                  permission of RUAG Space GmbH.
# ====================================================================== -->

import math
import matplotlib.pyplot as plt
import numpy as np
import sys, getopt

def main(argv):
    # inputs
    dist=111
    a_max=2
    v_max=30
    
    # state
    s=0
    v=0
    
    # debug state
    a_list = [0]
    v_list = [0]
    s_list = [0]

    # parse command line arguments
    try:
        opts, args = getopt.getopt(argv,"a:v:d:h",["amax", "vmax", "distance"])
    except getopt.GetoptError:
        print 'mctrl.py -a <a_max> -v <v_max> -d <distance>'
        sys.exit(2)

    for opt, arg in opts:
        if opt == '-h':
            print 'mctrl.py -a <a_max> -v <v_max> -d <distance>'
            sys.exit()
        elif opt in ("-a", "--amax"):
            a_max = int(arg)
        elif opt in ("-v", "--vmax"):
            v_max = int(arg)
        elif opt in ("-d", "--distance"):
            dist = int(arg)
    
    while True:
        
        # Compute deceleration distance based on current speed
        decel_steps = math.floor(v/a_max)
        s_decel = a_max * decel_steps * (decel_steps + 1) / 2
        if decel_steps * a_max != v:
            s_decel += v
    
        v_next = min(v_max, dist, v + a_max)
    
        if s + s_decel + v_next <= dist:
            # accelerate
            v = v_next
        elif s + s_decel + v <= dist:
            # keep speed
            v = v
        else:
            # decelerate
            if v > decel_steps * a_max:
                v = decel_steps * a_max
            else:
                v = v - a_max
    
        if v == 0:
            if s != dist:
                # didn't quite reach our target after deceleration
                # => accelerate again
                v = min(dist - s, a_max)
            else:
                # reached target
                v_list.append(0)
                break

        s += v
        
        v_list.append(v)        
        s_list.append(s)    
    
    
    # Velocity and Distance subplots
    f, (ax1, ax2) = plt.subplots(1, 2)    
    ax1.plot(v_list)
    ax1.set_title('Velocity')
    ax2.plot(s_list)
    ax2.set_title('Distance')
    plt.show()
    

if __name__ == "__main__":
   main(sys.argv[1:])
