#!/usr/bin/env python3
"""
This script loads a robot.yaml file and inserts it into the simulator.
"""

import os
import sys
import asyncio
from pyrevolve.SDF.math import Vector3
from pyrevolve import revolve_bot, parser
from pyrevolve.tol.manage import World
from pyrevolve.util.supervisor.supervisor_multi import DynamicSimSupervisor
from pyrevolve.evolution import fitness



async def run():
    """
    The main coroutine, which is started below.
    """

    arguments = parser.parse_args()

    if arguments.robot_name is not None:
        # this split will give errors on windows
        robot_file_path = "experiments/bodies/" + arguments.robot_name + ".yaml"

    if arguments.iterations is not None:
        # this split will give errors on windows
        iterations = int(arguments.iterations)

    #robot_file_path = "experiments/examples/yaml/spider.yaml"

    # Parse command line / file input arguments
    settings = parser.parse_args()

    # Start Simulator
    if settings.simulator_cmd != 'debug':
        simulator_supervisor = DynamicSimSupervisor(
            world_file=settings.world,
            simulator_cmd=settings.simulator_cmd,
            simulator_args=["--verbose"],
            plugins_dir_path=os.path.join('.', 'build', 'lib'),
            models_dir_path=os.path.join('.', 'models'),
            simulator_name='gazebo'
        )
        await simulator_supervisor.launch_simulator(port=settings.port_start)
        await asyncio.sleep(0.1)

    # Load a robot from yaml
    robot = revolve_bot.RevolveBot()
    robot.load_file(robot_file_path)
    robot.update_substrate()
    # robot._brain = BrainRLPowerSplines()

    # Connect to the simulator and pause
    connection = await World.create(settings, world_address=('127.0.0.1', settings.port_start))
    await asyncio.sleep(1)

    # Starts the simulation
    await connection.pause(False)

    # Insert the robot in the simulator
    #life_timeout = Fixed time a robot stays in the simulator
    robot_manager = await connection.insert_robot(robot, Vector3(0, 0, settings.z_start))

    robot_fitness = []
    for x in range(1, iterations):
        # Print robot fitness every second
        robot_fitness.append(fitness.displacement_velocity(robot_manager, robot))
        await asyncio.sleep(0.1)


    print("**The Robot Fitness is: ",str(sum(robot_fitness)/len(robot_fitness)))
