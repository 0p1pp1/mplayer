static int init(priv_t *priv, tvi_param_t *params);
static int uninit(priv_t *priv);
static int control(priv_t *priv, int cmd, void *arg);
static int start(priv_t *priv);
static int grab_video_frame(priv_t *priv, char *buffer, int len);
static int get_video_framesize(priv_t *priv);
static int grab_audio_frame(priv_t *priv, char *buffer, int len);
static int get_audio_framesize(priv_t *priv);

static tvi_functions_t functions =
{
    init,
    uninit,
    control,
    start,
    grab_video_frame,
    get_video_framesize,
    grab_audio_frame,
    get_audio_framesize
};

static tvi_handle_t *new_handle()
{
    tvi_handle_t *h = (tvi_handle_t *)malloc(sizeof(tvi_handle_t));

    if (!h)
	return(NULL);
    h->priv = (priv_t *)malloc(sizeof(priv_t));
    if (!h->priv)
    {
	free(h);
	return(NULL);
    }
    memset(h->priv, 0, sizeof(priv_t));
    h->info = &info;
    h->functions = &functions;
    h->params = NULL;
    h->seq = 0;
    return(h);
}

static void free_handle(tvi_handle_t *h)
{
    if (h->priv)
	free(h->priv);
    if (h)
	free(h);
}
