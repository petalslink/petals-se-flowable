/**
 * Copyright (c) 2017-2026 Linagora
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
package org.ow2.petals.flowable.utils.test;

import static org.ow2.petals.flowable.utils.test.Assert.assertCurrentIntermediateCatchMessageEvent;

import java.util.Map;

import org.flowable.engine.RuntimeService;
import org.flowable.engine.runtime.Execution;

/**
 * Utility class to manage Flowable process events
 * 
 * @author Christophe DENEUX - Linagora
 */
public class Signal {

    /**
     * Signal an intermediate catch message event.
     * 
     * @param processInstanceId
     *            The identifier of the process instance waiting the intermediate catch message event
     * @param messageEventName
     *            The message name of the intermediate catch message event
     * @param runtimeService
     *            The Flowable's runtime service used to request the Flowable engine about process instances.
     */
    public static void signalIntermediateCatchMessageEvent(final String processInstanceId,
            final String messageEventName, final RuntimeService runtimeService) {

        Signal.signalIntermediateCatchMessageEvent(processInstanceId, messageEventName, null, runtimeService);
    }

    /**
     * <p>
     * Signal an intermediate catch message event, providing variables to set on process instance.
     * </p>
     * <p>
     * This method will do a search query to retrieve the execution associated to the intermediate catch message event.
     * To avoid to do a new search query, use
     * {@link #signalIntermediateCatchMessageEvent(Execution, String, RuntimeService)}.
     * </p>
     * 
     * @param processInstanceId
     *            The identifier of the process instance waiting the intermediate catch message event
     * @param messageEventName
     *            The message name of the intermediate catch message event
     * @param variables
     *            Process variables to set signaling the intermediate catch message event
     * @param runtimeService
     *            The Flowable's runtime service used to request the Flowable engine about process instances.
     */
    public static void signalIntermediateCatchMessageEvent(final String processInstanceId,
            final String messageEventName, final Map<String, Object> variables, final RuntimeService runtimeService) {

        final Execution execution = assertCurrentIntermediateCatchMessageEvent(processInstanceId, messageEventName,
                runtimeService);

        if (variables == null) {
            runtimeService.messageEventReceived(messageEventName, execution.getId());
        } else {
            runtimeService.messageEventReceived(messageEventName, execution.getId(), variables);
        }
    }

    /**
     * Signal an intermediate catch message event.
     * 
     * @param execution
     *            The {@link Execution} associated to the intermediate catch message event in wait.
     * @param messageEventName
     *            The message name of the intermediate catch message event
     * @param runtimeService
     *            The Flowable's runtime service used to request the Flowable engine about process instances.
     */
    public static void signalIntermediateCatchMessageEvent(final Execution execution, final String messageEventName,
            final RuntimeService runtimeService) {

        Signal.signalIntermediateCatchMessageEvent(execution, messageEventName, null, runtimeService);

    }

    /**
     * Signal an intermediate catch message event, providing variables to set on process instance.
     * 
     * @param execution
     *            The {@link Execution} associated to the intermediate catch message event in wait.
     * @param messageEventName
     *            The message name of the intermediate catch message event
     * @param variables
     *            Process variables to set signaling the intermediate catch message event
     * @param runtimeService
     *            The Flowable's runtime service used to request the Flowable engine about process instances.
     */
    public static void signalIntermediateCatchMessageEvent(final Execution execution, final String messageEventName,
            final Map<String, Object> variables, final RuntimeService runtimeService) {

        if (variables == null) {
            runtimeService.messageEventReceived(messageEventName, execution.getId());
        } else {
            runtimeService.messageEventReceived(messageEventName, execution.getId(), variables);
        }
    }
}
