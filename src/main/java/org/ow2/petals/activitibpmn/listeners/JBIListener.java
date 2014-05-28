/**
 * Copyright (c) 2014-2014 Linagora
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
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.listeners;

import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.component.framework.listener.AbstractJBIListener;

/**
 * Listens to messages incoming from inside Petals.
 * <p>
 * This class is in charge of processing messages coming from a Petals
 * consumer. These messages can be requests (in messages) or acknowledgments (ACK).
 * </p>
 * <p>
 * Depending on the invoked operation, the message exchange pattern (MEP) and
 * the component's logic, this class may build and send a response.
 * </p>
 * 
 * @author bescudie
 */
public class JBIListener extends AbstractJBIListener {

    /*
     * (non-Javadoc)
     * @see org.ow2.petals.component.framework.listener.AbstractJBIListener
     * #onJBIMessage(org.ow2.petals.component.framework.api.message.Exchange)
     */
    @Override
	public boolean onJBIMessage( Exchange exchange ) {

    	// True to let the CDK close the exchange.
    	// False to explicitly return the exchange.
        return true;
    }
}
