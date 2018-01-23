/**
 * Copyright (c) 2015-2018 Linagora
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

import org.flowable.engine.common.api.delegate.event.FlowableEventListener;
import org.flowable.engine.delegate.event.FlowableEngineEventType;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;

/**
 * Abstract class logging directly a MONIT trace on Flowable events
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public abstract class AbstractMonitDirectLoggerEventListener extends AbstractMonitLoggerEventListener implements
        FlowableEventListener {

    protected static final String MISSING_VARIABLE_PATTERN = "Unable to find the variable '%s' into the process instance '%s'.";

    public AbstractMonitDirectLoggerEventListener(final FlowableEngineEventType listenEventType, final Logger log) {
        super(listenEventType, log);
    }

    @Override
    protected void flushLogData(final AbstractFlowLogData logData) {
        this.log.log(Level.MONIT, "", logData);
    }
}
