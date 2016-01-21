gore-and-ash-glfw
====================

The module provides events for GLFW input for [Gore&Ash](https://github.com/Teaspot-Studio/gore-and-ash) engine.

Installing
==========

Add following to your `stack.yml` to `packages` section:
```yaml
- location:
    git: https://github.com/Teaspot-Studio/gore-and-ash-glfw.git
    commit: <PLACE HERE FULL HASH OF LAST COMMIT> 
```

When defining you application stack, add `GLFWT`:
``` haskell
type AppStack = ModuleStack [GLFWT, ... other modules ... ] IO
```

And derive `MonadGLFWInput` for your resulting `AppMonad`:
``` haskell
newtype AppMonad a = AppMonad (AppStack a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadGLFWInput)
```