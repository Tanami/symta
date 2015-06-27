use ecs

component &&xyz/[0 0 0]
component &&direction/[0 0 0]
component &scale/1
component wheel{xyz direction}
component wheels/wheel.4
component chassis
component engine/'gasoline'
component car{xyz direction chassis engine wheels}


ECS = ecs 1024

Car = ECS.new{car}

Car.wheels.1.xyz <= [123 456 789]


say Car.wheels.1.xyz
say Car.engine

// dump whole ECS as text
say ECS.text

