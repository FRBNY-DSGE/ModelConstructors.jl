# ModelConstructors.jl

[![Build Status](https://travis-ci.com/FRBNY-DSGE/ModelConstructors.jl.svg?branch=master)](https://travis-ci.com/FRBNY-DSGE/ModelConstructors.jl)
[![](https://img.shields.io/badge/docs-latest-blue.svg)](https://frbny-dsge.github.io/ModelConstructors.jl/latest)

This package contains the building blocks of model objects, such as `Parameter`, `Observable`, `Setting`, and `State` types. You may define any custom model, so long as it has parameters. The model object is used in both [DSGE.jl](https://github.com/FRBNY-DSGE/DSGE.jl) and [SMC.jl](https://github.com/FRBNY-DSGE/SMC.jl).

See `/docs/examples` for examples of how to construct your custom model, then use [SMC.jl](https://github.com/FRBNY-DSGE/SMC.jl) to estimate it (available for any type of model) and [DSGE.jl](https://github.com/FRBNY-DSGE/DSGE.jl) to construct forecasts, shock decompositions, impulse responses, etc. (available for DSGE models).

## Installation
`ModelConstructors.jl` is a registered Julia package in the [`General`](https://github.com/JuliaRegistries/General) registry.  To install, open your Julia REPL, type `]` (enter package manager), and run

```julia
pkg> add ModelConstructors
```

## Versioning
`ModelConstructors.jl` is currently compatible with Julia `v0.7`, `v1.0`, `v1.1`, `v1.2`, and `v1.3`.

## Disclaimer
Copyright Federal Reserve Bank of New York. You may reproduce, use, modify, make derivative works of, and distribute and this code in whole or in part so long as you keep this notice in the documentation associated with any distributed works. Neither the name of the Federal Reserve Bank of New York (FRBNY) nor the names of any of the authors may be used to endorse or promote works derived from this code without prior written permission. Portions of the code attributed to third parties are subject to applicable third party licenses and rights. By your use of this code you accept this license and any applicable third party license.

THIS CODE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT ANY WARRANTIES OR CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION ANY WARRANTIES OR CONDITIONS OF TITLE, NON-INFRINGEMENT, MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, EXCEPT TO THE EXTENT THAT THESE DISCLAIMERS ARE HELD TO BE LEGALLY INVALID. FRBNY IS NOT, UNDER ANY CIRCUMSTANCES, LIABLE TO YOU FOR DAMAGES OF ANY KIND ARISING OUT OF OR IN CONNECTION WITH USE OF OR INABILITY TO USE THE CODE, INCLUDING, BUT NOT LIMITED TO DIRECT, INDIRECT, INCIDENTAL, CONSEQUENTIAL, PUNITIVE, SPECIAL OR EXEMPLARY DAMAGES, WHETHER BASED ON BREACH OF CONTRACT, BREACH OF WARRANTY, TORT OR OTHER LEGAL OR EQUITABLE THEORY, EVEN IF FRBNY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES OR LOSS AND REGARDLESS OF WHETHER SUCH DAMAGES OR LOSS IS FORESEEABLE.
