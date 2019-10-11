/**
 * Copyright (c) 2015-2019 Linagora
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

import org.flowable.common.engine.api.delegate.event.FlowableEngineEventType;
import org.flowable.common.engine.api.delegate.event.FlowableEvent;
import org.flowable.common.engine.api.delegate.event.FlowableEventListener;
import org.ow2.petals.component.framework.AbstractComponent;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;

/**
 * Abstract class logging a MONIT trace on Flowable events
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public abstract class AbstractMonitLoggerEventListener extends AbstractEventListener implements FlowableEventListener {

    public AbstractMonitLoggerEventListener(final FlowableEngineEventType listenEventType,
            final AbstractComponent component) {
        super(listenEventType, component);
    }

    protected abstract AbstractFlowLogData createLogData(final FlowableEvent event);

    protected abstract void flushLogData(final AbstractFlowLogData logData);

    @Override
    public void processEvent(final FlowableEvent event) {

        final AbstractFlowLogData flowLogData;
        if ((flowLogData = this.createLogData(event)) != null) {
            this.flushLogData(flowLogData);
        }
    }
}
