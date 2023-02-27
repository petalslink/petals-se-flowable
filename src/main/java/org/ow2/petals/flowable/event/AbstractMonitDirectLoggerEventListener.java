/**
 * Copyright (c) 2015-2023 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_EXT_FLOW_TRACING_ACTIVATION_STATE;

import java.util.Map;

import org.flowable.common.engine.api.delegate.event.FlowableEngineEventType;
import org.flowable.common.engine.api.delegate.event.FlowableEventListener;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.AbstractComponent;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;
import org.ow2.petals.flowable.FlowableSEConstants.Flowable;

/**
 * Abstract class logging directly a MONIT trace on Flowable events
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public abstract class AbstractMonitDirectLoggerEventListener extends AbstractMonitLoggerEventListener
        implements FlowableEventListener {

    protected static final String MISSING_VARIABLE_PATTERN = "Unable to find the variable '%s' into the process instance '%s'.";

    public AbstractMonitDirectLoggerEventListener(final FlowableEngineEventType listenEventType,
            final AbstractComponent component) {
        super(listenEventType, component);
    }

    @Override
    protected void flushLogData(final AbstractFlowLogData logData) {
        this.log.log(Level.MONIT, "", logData);
    }

    /**
     * <p>
     * Retrieve the value of the flow tracing activation according to the process variable
     * {@value Flowable#VAR_PETALS_EXT_FLOW_TRACING_ACTIVATION_STATE} defined at process instance level, and the
     * parameter 'activate-flow-tracing' defined component level.
     * </p>
     * 
     * @param processVariables
     *            Process instance variables
     * @return The flow tracing activation state.
     */
    protected boolean isFlowTracingEnabled(final Map<String, Object> processVariables) {
        final boolean isFlowTracingStateDefinedAtProcessLevel = processVariables
                .containsKey(VAR_PETALS_EXT_FLOW_TRACING_ACTIVATION_STATE);
        return (isFlowTracingStateDefinedAtProcessLevel
                && (boolean) processVariables.get(VAR_PETALS_EXT_FLOW_TRACING_ACTIVATION_STATE))
                || (!isFlowTracingStateDefinedAtProcessLevel && this.component.isFlowTracingActivated());
    }
}
