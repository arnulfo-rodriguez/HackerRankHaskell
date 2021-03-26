@Composition
public class Pipeline {

    @Step(1)
    Observable<X> getX();
    @Step(2)
    Observable<U> getY(Observable<X> prev);
    @Step(3)
        int finish(Observable<Y>);

        finish(getY(getX()));


        class GetX{
            Observable<X> actualGetX(){........}

            GetY getY() {
                new GetY(actualGetX())
            }

        }

        start()
            .getX()
            .getY()
            .finish()
}
