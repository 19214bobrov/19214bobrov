data Group = ToGroup {
                      gpName :: String,
                      gpMembers :: [String],
                      gpSong :: [Song]
                      } deriving Show


data Album = ToAlbum {
                      amName :: String,
                      amSong :: [String]
                      } deriving Show


data Song = ToSong {
                    sgName :: String
                    } deriving Show


data User = ToUser {
                    urName :: String,
                    urSongs :: [Song],
                    urAlbums :: [Album],
                    urGroups :: [Group]
                    } deriving Show


newGroup :: User -> Group -> User
newGroup user group = ToUser (urName user) (urSongs user) (urAlbums user) (urGroups user ++ [group])


newAlbum :: User -> Album -> User
newAlbum user album = ToUser (urName user) (urSongs user) (urAlbums user ++ [album]) (urGroups user)


newSong :: User -> Song -> User
newSong user song = ToUser (urName user) (urSongs user ++ [song]) (urAlbums user) (urGroups user)


searchSongByName :: User -> String -> [Song]
searchSongByName user name = [song | song <- (urSongs user), sgName song == name]
