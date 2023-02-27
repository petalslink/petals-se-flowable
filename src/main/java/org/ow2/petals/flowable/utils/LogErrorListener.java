/**
 * Copyright (c) 2019-2023 Linagora
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
package org.ow2.petals.flowable.utils;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.transform.ErrorListener;
import javax.xml.transform.SourceLocator;
import javax.xml.transform.TransformerException;

/**
 * An error listener used to resolve line numbers and log details about errors occuring in an XSL.
 * 
 * @author Christophe DENEUX - Linagora
 */
// TODO: Should be mutualized with the PEtals BC Rest where it is also used.
public class LogErrorListener implements ErrorListener {

    /**
     * The component's logger.
     */
    private final Logger logger;

    /**
     * The prefix used for every logged message.
     */
    private final String logPrefix;

    /**
     * Constructor.
     * 
     * @param logger
     *            the component's logger
     * @param suName
     *            the SU name (used to prefix log messages)
     */
    public LogErrorListener(final Logger logger, final String suName) {
        this.logger = logger;
        this.logPrefix = suName + ": ";
    }

    @Override
    public void error(final TransformerException e) throws TransformerException {
        log(e, "An error", Level.SEVERE, true);
    }

    @Override
    public void fatalError(final TransformerException e) throws TransformerException {
        log(e, "A fatal error", Level.SEVERE, true);
    }

    @Override
    public void warning(final TransformerException e) throws TransformerException {
        log(e, "A warning", Level.WARNING, false);
    }

    private void log(final TransformerException e, final String errorType, final Level level, final boolean rethrow)
            throws TransformerException {

        if (this.logger.isLoggable(Level.FINE)) {
            this.logger.log(Level.FINE, this.logPrefix, e);
        } else if (this.logger.isLoggable(level)) {
            int line = -1;
            final SourceLocator locator = e.getLocator();
            if (locator != null)
                line = locator.getLineNumber();

            final String msg = errorType + " was found: " + e.getMessage() + " (line: "
                    + ((line == -1) ? "unresolved" : line) + ").";
            this.logger.log(level, this.logPrefix + msg);
        }

        if (rethrow) {
            throw e;
        }
    }
}
