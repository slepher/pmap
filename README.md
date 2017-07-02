A library includes

* parallel library which is pmap

* async monad based on erlando which is async_m

* pmap and async_m will be rewritten by async_t

* async cc result monad transformer which is async_r_t

  * type AsyncRT s m a = StateT s (ReaderT Reference (ReaderT CallbacksGS m)) a
  
* async monad transformer which is async_t

  * type AsyncT s r m a = ReplyT Message Error (ContT r (AsyncRT s m)) a

