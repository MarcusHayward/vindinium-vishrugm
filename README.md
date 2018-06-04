Scala starter pack for the Vindinium AI challenge

## Develop / run

```
sbt -Dkey=mySecretKey

Training mode, 30 turns:
> run training 30

Arena mode, 10 games:
> run arena 10
```

## Package as single jar

From sbt, run:

```
> one-jar
```

You can now run the application without sbt:

```
java -Dkey=mySecretKey -jar target/scala-2.10/vindinium-bot_2.10-0.1-one-jar.jar arena 3
```

A bash alias can make it prettier:

```
alias vindinium="java -Dkey=mySecretKey -jar target/scala-2.10/vindinium-bot_2.10-0.1-one-jar.jar"
```

You can now run your bot like that:

```
vindinium arena 3
```


## Docker

To run the docker application:
```
docker-compose run --rm scalabot -Dserver=http://159.65.169.154:9000/ -Dkey=<key>
run training 50 m5
```

