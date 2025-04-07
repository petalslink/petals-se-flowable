/**
 * Copyright (c) 2015-2025 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.event;

import java.util.logging.Logger;

import org.flowable.common.engine.api.delegate.event.FlowableEngineEventType;
import org.flowable.common.engine.api.delegate.event.FlowableEvent;
import org.flowable.common.engine.api.delegate.event.FlowableEventListener;
import org.ow2.petals.component.framework.AbstractComponent;

/**
 * Abstract class managing Flowable event
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public abstract class AbstractEventListener implements FlowableEventListener {

    protected final AbstractComponent component;

    @SuppressWarnings("squid:S1312")
    protected final Logger log;

    private final FlowableEngineEventType listenEventType;

    public AbstractEventListener(final FlowableEngineEventType listenEventType, final AbstractComponent component) {

        assert component != null;
        assert listenEventType != null;

        this.component = component;
        this.log = component.getLogger();
        this.listenEventType = listenEventType;
    }

    protected abstract void processEvent(final FlowableEvent event);

    @Override
    public void onEvent(final FlowableEvent event) {

        assert event != null;

        if (event.getType() == this.listenEventType) {

            this.processEvent(event);

        } else {
            this.log.warning("Unexpected event type '" + event.getType().name() + "'. Event discarded !");
        }

    }

    @Override
    public boolean isFailOnException() {
        return false;
    }

    public FlowableEngineEventType getListenEventType() {
        return this.listenEventType;
    }

    @Override
    public boolean isFireOnTransactionLifecycleEvent() {
        return false;
    }

    @Override
    public String getOnTransaction() {
        return null;
    }

}
