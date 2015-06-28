use ecs


//say hello
//component xyz.3/0

/*
component xyz.3/0
ECS = ecs 1024
O = ECS.new{xyz,[1 2 3]}
say O.xyz


component xyz.3/0
component direction.3/0
component scale/1
component wheel{xyz direction}
component wheels.4{wheel}
component chassis
component engine/'gasoline'
component car{xyz direction chassis engine wheels}


ECS = ecs 1024

Car = ECS.new{car}

//Car.wheels.1.xyz <= [123 456 789]


say Car.wheels.1.xyz
say Car.engine

// dump whole ECS as text
say ECS.text

*/