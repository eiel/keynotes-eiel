{-# LANGUAGE OverloadedStrings,QuasiQuotes #-}

import qualified Aws
import qualified Aws.S3 as S3
import qualified Data.Text as T
import qualified Data.ByteString as B (concat)
import qualified Data.ByteString.Lazy as B (toChunks, putStr)
import Network.HTTP.Conduit (withManager, RequestBody( RequestBodyBS ))
import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Data.Time.Clock

bucketName = "keynotes.eiel.info"

main :: IO ()
main = do
  html <- createHtml
  putObject . RequestBodyBS . B.concat . B.toChunks $ html
  return ()

createHtml = do
  infos <- getObjectInfo
  return . toHtml . filter isPublicObject $ infos
    where
      isPublicObject S3.ObjectInfo { S3.objectKey = key } =
        ".zip" `T.isSuffixOf` key


getObjectInfo :: IO [S3.ObjectInfo]
getObjectInfo = do
  {- Set up AWS credentials and the default configuration. -}
  cfg <- Aws.baseConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  {- Set up a ResourceT region with an available HTTP manager. -}
  withManager $ \mgr -> do
    {- Create a request object with S3.getObject and run the request with pureAws. -}
    S3.GetBucketResponse { S3.gbrContents = objectInfo } <-
      Aws.pureAws cfg s3cfg mgr $
        S3.getBucket bucketName

    return objectInfo

putObject body = do
  cfg <- Aws.baseConfiguration
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  withManager $ \mgr -> do
    let object = S3.putObject bucketName "index.html" body
    Aws.pureAws cfg s3cfg mgr $ object { S3.poContentType = Just "text/html" }

list S3.ObjectInfo { S3.objectKey = key } =
  [shamlet|
<li>
  <a href=#{key}>#{name}
|]
  where name = key

toHtml infos = renderHtml $ [shamlet|
$doctype 5
<html>
  <head>
    <meta charset="utf-8">
    <title>keynotes.eiel.info
  <body>
    <h1>スライドの保存場所
    ここは自分が作成したスライドの元データの格納庫です。スライドがみたいだけの場合は <a href="http://www.slideshare.net/TomohikoHimura/presentations" target="_black">Slide Share</a> や <a href="https://speakerdeck.com/eiel" target="_blank">Speaker Deck</a> を確認してください。
    <ul>
    $forall info <- infos
      ^{list info}
|]

sampleHtml :: IO ()
sampleHtml = sampleInfo >>= return . toHtml . (:[]) >>= B.putStr

sampleInfo :: IO S3.ObjectInfo
sampleInfo = do
  time <- getCurrentTime
  return S3.ObjectInfo {
   S3.objectKey = "hoge",
   S3.objectLastModified = time,
   S3.objectETag = "etag",
   S3.objectSize = 100,
   S3.objectStorageClass = S3.Standard,
   S3.objectOwner = Just $ S3.UserInfo {
     S3.userId = "eiel",
     S3.userDisplayName = "eiel"
     }
   }
